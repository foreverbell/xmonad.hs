{-# LANGUAGE RecordWildCards #-}

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.CycleWindows
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.GridSelect
import qualified XMonad.Actions.Search as S
import           XMonad.Config.Xfce
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet as StackSet
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           Control.Applicative
import           Control.Monad (void)
import           Data.List (find, isSuffixOf)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.IORef
import           Text.Printf (printf)
import           System.Posix.Process (executeFile)

spawnBash :: MonadIO m => String -> m ()
spawnBash x = void $ xfork $ executeFile "/bin/bash" False ["-c", x] Nothing

spawnNamedSelected :: GSConfig String -> [(String, String)] -> X ()
spawnNamedSelected conf lst = gridselect conf lst >>= flip whenJust spawnBash

data SysConfig = SysConfig {
  sysTerminal  :: String
, sysBrowser   :: String
, sysBacklight :: IORef Int
, sysVolume    :: IORef (Int, Bool)
, sysFcitx     :: IORef Bool
}

testCmdExistence :: FilePath -> IO Bool
testCmdExistence cmd = do
  output <- runProcessWithInput "sh" ["-c", "which " ++ cmd] ""
  return $ not $ null output

getClipboard :: IO String
getClipboard = runProcessWithInput "sh" ["-c", "xclip -o"] ""

mkWorkspaceName :: Int -> String
mkWorkspaceName = show

updateBacklight :: IORef Int -> IO ()
updateBacklight backlight = do
  b <- readIORef backlight
  spawn $ "xbacklight -time 0 -set " ++ show b

updateVolume :: IORef (Int, Bool) -> IO ()
updateVolume volume = do
  (v, m) <- readIORef volume
  spawn $ printf "amixer -q -D pulse set Master %d%% %s" v (if m then "mute" else "unmute")

toggleFcitx :: IORef Bool -> IO ()
toggleFcitx fcitx = do
  b <- readIORef fcitx
  spawn $ if b then "killall fcitx" else "fcitx"
  writeIORef fcitx (not b)

myFont :: Int -> String
myFont = printf "xft:Ubuntu Mono:size=%d"

myMouses XConfig {XMonad.modMask = modM} = M.fromList
  [ ((modM, button1), \w -> focus w >> Flex.mouseWindow Flex.position w)
  , ((modM, button2), \w -> focus w >> Flex.mouseWindow Flex.linear w)
  , ((modM, button3), \w -> focus w >> Flex.mouseWindow Flex.resize w)
  ]

myKeys SysConfig {..} = \conf -> mkKeymap conf $
  [ ("M-" ++ show i, windows $ StackSet.greedyView (mkWorkspaceName i)) | i <- [1 .. 9] ] ++
  [ ("M-S-" ++ show i, do
      windows $ StackSet.shift (mkWorkspaceName i)
      windows $ StackSet.greedyView (mkWorkspaceName i)) | i <- [1 .. 9] ] ++
  [ ("M-<Tab>", windows StackSet.focusDown)
  , ("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab)
  , ("M-S-c", spawn "xkill")
  , ("M-<Delete>", kill)
  , ("M1-<F4>", kill)
  , ("<Print>", do
      spawn "scrot '%Y-%m-%d:%H:%M:%S_$wx$h_screen_png.png' -e 'mv $f /tmp/'"
      spawn "scrot '%Y-%m-%d:%H:%M:%S_$wx$h_screen_jpg.jpg' -e 'mv $f /tmp/'")
  , ("M1-<Print>", do
      spawn "scrot -u '%Y-%m-%d:%H:%M:%S_$wx$h_window_png.png' -e 'mv $f /tmp/'"
      spawn "scrot -u '%Y-%m-%d:%H:%M:%S_$wx$h_window_jpg.jpg' -e 'mv $f /tmp/'")
  , ("M-<F11>", sendMessage ToggleStruts)
  , ("M-q", spawnNamedSelected def { gs_font = myFont 11, gs_cellwidth = 170, gs_cellheight = 50 }
      [ ("[session] logout", "xfce4-session-logout")
      , ("[session] lock", "xflock4")
      , ("[xmonad] recompile", "stack exec xmonad -- --recompile && stack exec xmonad -- --restart")
      , ("[xmonad] restart", "stack exec xmonad -- --restart")
      ]
    )
  , ("M-p", spawn "gmrun")
  , ("M-S-p", inputPrompt myXPConfig "Run [terminal]" ?+ \cmd -> spawn (runInTerminal cmd))
  , ("M-r", spawn sysTerminal)
  , ("M-S-l", spawn "xflock4")
  , ("M-f", liftIO (toggleFcitx sysFcitx))
  , ("M-g", spawnNamedSelected def { gs_font = myFont 11 }
      [ ("terminal", sysTerminal)
      , ("web-browser", sysBrowser)
      , ("firefox", "firefox")
      , ("explorer", "thunar")
      , ("gedit", "gedit")
      , ("okular", "okular")
      , ("eclipse", "eclipse-ide")
      , ("coqide", "coqide")
      ]
    )
  , ("M-<L>", prevWS)
  , ("M-<R>", nextWS)
  , ("M-<U>", shiftToPrev >> prevWS)
  , ("M-<D>", shiftToNext >> nextWS)
  , ("M-t", withFocused $ windows . StackSet.sink)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-i", sendMessage Shrink)
  , ("M-o", sendMessage Expand)
  , ("M-m", withFocused minimizeWindow)
  , ("M-S-m", sendMessage RestoreNextMinimizedWin)
  , ("M-S-<F1>", setVolume id not)
  , ("M-S-<F2>", setVolume (+ (-5)) id)
  , ("M-S-<F3>", setVolume (+ 5) id)
  , ("M-S-<F5>", setBacklight (+ (-10)))
  , ("M-S-<F6>", setBacklight (+ 10))
  ] ++ searchKeys sysBrowser
  where
    runInTerminal cmd = sysTerminal ++ " -e " ++ cmd
    clamp x | x < 0 = 0
            | x > 100 = 100
            | otherwise = x
    setVolume vol mute = liftIO $ do
      (v, m) <- readIORef sysVolume
      writeIORef sysVolume (clamp (vol v), mute m)
      updateVolume sysVolume
    setBacklight back = liftIO $ do
      b <- readIORef sysBacklight
      writeIORef sysBacklight (clamp (back b))
      updateBacklight sysBacklight

myManageHook :: ManageHook
myManageHook = composeAll
  [ (isSuffixOf "VLC media player" <$> title) --> moveTo 9
  , (isSuffixOf "Parole Media Player" <$> title) --> moveTo 9
  , title =? "xfce4-notifyd" --> doIgnore
  , isFullscreen --> doFullFloat
  , isDialog --> doDialogFloat
  ]
  where
    moveTo i = let ws = mkWorkspaceName i in doF $ StackSet.greedyView ws . StackSet.shift ws
    doDialogFloat = doFloatDep move
      where
        -- We need to resize our floating window a bit larger, since XMonad's way of handling
        -- floating window is a bit .. problematic.
        move (StackSet.RationalRect _ _ w h) = StackSet.RationalRect ((1-w')/2) ((1-h')/2) w' h'
          where
            w' = min 1 (w+d)
            h' = min 1 (h+d)
            d = 0.05

myLayout = avoidStruts $ minimize $ smartBorders composed
  where
    tiled = ResizableTall 1 0.03 0.5 []
    full = Full
    composed = full ||| tiled

-- TODO: Consider XMonad.Prompt.insertString
myPromptKeymap = M.union defaultXPKeymap $ M.fromList
  [ ((controlMask, xK_v), setInput =<< liftIO getClipboard)
  ]

myXPConfig = amberXPConfig
  { font          = myFont 11
  , historySize   = 12
  , historyFilter = deleteConsecutive
  , promptKeymap  = myPromptKeymap
  }

searchKeys browser = [ ("M-/ " ++ name, promptSearch e) | e@(S.SearchEngine name _, _) <- engines ]
  where
    promptSearch (S.SearchEngine _ site, abbr) = inputPrompt myXPConfig ("Search [" ++ abbr ++ "]") ?+ \query -> S.search browser site query
    engines = mk <$> [ ("h", "hoogle", "http://www.haskell.org/hoogle/?hoogle=")
                     , ("g", "google", "https://www.google.com.hk/search?num=100&q=")
                     , ("w", "wikipedia", "https://en.wikipedia.org/wiki/Special:Search?go=Go&search=")
                     , ("s", "scholar", "https://scholar.google.com/scholar?q=")
                     , ("d", "dict", "http://dict.youdao.com/search?q=")
                     ]
    mk (n, a, s) = (S.searchEngine n s, a)

lookupAvailableCmd :: [String] -> IO String
lookupAvailableCmd cs = do
  existence <- mapM (\b -> do { e <- testCmdExistence b; return (b, e) }) cs
  return $ fromMaybe [] $ fst <$> find snd existence

mySysConfig :: IO SysConfig
mySysConfig = do
  sysBrowser <- lookupAvailableCmd ["chromium-browser", "google-chrome", "firefox"]
  sysTerminal <- lookupAvailableCmd ["xfce4-terminal", "alacritty", "uxterm", "gnome-terminal"]
  sysBacklight <- do
    output <- runProcessWithInput "xbacklight" ["-get"] ""
    newIORef $ floor (read output :: Double)
  sysVolume <- newIORef (25, True)
  sysFcitx <- newIORef False
  return SysConfig { .. }

main :: IO ()
main = do
  sysconf@SysConfig {..} <- mySysConfig
  updateBacklight sysBacklight
  updateVolume sysVolume
  let xconf = xfceConfig
  xmonad $ ewmh xconf
    { terminal = sysTerminal
    , modMask = mod4Mask
    , workspaces = map mkWorkspaceName [1 .. 9]
    , keys = myKeys sysconf
    , borderWidth = 2
    , focusFollowsMouse = False
    , mouseBindings = myMouses
    , manageHook = myManageHook <+> manageHook xconf
    , layoutHook = showWName myLayout
    , startupHook = setWMName "LG3D"
    , normalBorderColor  = "#dbdbdb"
    , focusedBorderColor = "#3939ff"
    , handleEventHook = fullscreenEventHook
    }
