{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad                       (when)
import           Data.Foldable                       (traverse_)
import           Data.Map                            (member)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust, isNothing)
import           Data.Monoid                         (All (All))
import           IfMaxAlt
import           Prelude
import           SideBorderDecoration
import           XMonad
import           XMonad.Actions.CopyWindow           (copyToAll,
                                                      killAllOtherCopies)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Minimize             (maximizeWindow,
                                                      maximizeWindowAndFocus,
                                                      minimizeWindow,
                                                      withLastMinimized)
import           XMonad.Actions.SpawnOn              (manageSpawn, spawnAndDo)
import           XMonad.Actions.WindowBringer        (WindowBringerConfig (windowTitler),
                                                      bringMenuConfig,
                                                      gotoMenuConfig)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicProperty        (dynamicPropertyChange)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (Newer),
                                                      Position (End, Master),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat, isDialog)
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.WindowSwallowing       (swallowEventHook)
import           XMonad.Layout.BoringWindows         (boringWindows, focusDown,
                                                      focusMaster, focusUp)
import           XMonad.Layout.ImageButtonDecoration (defaultThemeWithImageButtons,
                                                      imageButtonDeco)
import           XMonad.Layout.LayoutHints           (hintsEventHook,
                                                      layoutHintsToCenter)
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize              (minimize)
import           XMonad.Layout.ResizableTile         (MirrorResize (..),
                                                      ResizableTall (..))
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.Simplest              (Simplest (Simplest))
import qualified XMonad.StackSet                     as W
import           XMonad.StackSet                     (peek)
import           XMonad.Util.Cursor
import           XMonad.Util.Run                     (runProcessWithInput)
import           XMonad.Util.Themes
import           XMonad.Util.WorkspaceCompare        (WorkspaceSort,
                                                      filterOutWs)

main = xmonad
     . addEwmhWorkspaceSort (filterEmpty <$> gets windowset)
     . ewmh
     . ewmhFullscreen
     . docks
     $ conf

conf = desktopConfig
     { terminal           = "qterminal"
     , modMask            = mod4Mask
     , startupHook        = setWMName "LG3D" <> setDefaultCursor xC_left_ptr
     , keys               = myKeys
     , borderWidth        = 0
     , layoutHook         = layoutHook'
     , manageHook         = manageHook'
     , handleEventHook    = handleEventHook'
     , focusFollowsMouse  = False
     }

layoutHook' =
  let
    tiled = ResizableTall 1 (1 / 20) (103 / 200) []
    decor (x :: l Window) = sideBorderLayout' blueBorder U x
  --  decor = imageButtonDeco shrinkText clearlooks
  in
    avoidStruts
  . maximizeWithPadding 0
  . minimize
  . boringWindows
  . layoutHintsToCenter
  . ifMax 1 Simplest
  . decor
  $ tiled ||| Full ||| Mirror tiled

handleEventHook' :: Event -> X All
handleEventHook' = composeAll
  [ minimizeEventHook
  , hintsEventHook
  , showDesktopEventHook
  , dynamicPropertyChange "WM_CLASS" (className =? "Spotify" --> doShift "9")
  , focusHook
  , swallowEventHook (className =? "qterminal") (not <$> isDialog)
  ]

-- Ordering is important
manageHook' :: ManageHook
manageHook' = composeAll
  [ manageSpawn
  , placeHook $ smart (0.5, 0.5)
  , className =? "thunderbird" --> doShift "8"
  , className =? "qBittorrent" --> doShift "7"
  , className =? "discord" --> doShift "9"
  , isDialog --> doFloat
  , smartInsert
  ]

isFocus :: Window -> X Bool
isFocus w = withWindowSet $ \ws ->
    case peek ws of
      (Just w') -> pure (w' == w)
      _         -> pure False


raiseWin :: Window -> WindowSet -> WindowSet
raiseWin w = W.modify' f
  where
    err = error "raiseWin: multiple windows matching Window (XID)"
    f (W.Stack t ls rs)
      | w == t      = W.Stack t [] (ls ++ rs)
      | w `elem` ls =  case break (==w) ls of
                         ([_], rest) -> W.Stack t (w:rest) rs
                         _           -> err
      | otherwise   =  case break (==w) rs of
                         ([_], rest) -> W.Stack t (w:ls) rest
                         _           -> err


smartInsert :: ManageHook
smartInsert = willFloat >>= \float ->
  insertPosition (if float then Master else End) Newer


filterEmpty :: WindowSet -> WorkspaceSort
filterEmpty ss =
  let
    isEmpty w = isNothing (W.stack w)
             && W.currentTag ss /= W.tag w
  in
    filterOutWs
  . map W.tag
  . filter isEmpty
  $ W.workspaces ss

focusHook :: Event -> X All
focusHook = \case
  ClientMessageEvent {ev_message_type, ev_window} -> do
    p <- (==ev_message_type) <$> getAtom "_NET_ACTIVE_WINDOW" <&&> isFloat ev_window
    let raiseFloat = windows $ raiseWin ev_window
    when p raiseFloat
    pure $ All True
  _ ->
    pure $ All True

isFloat :: Window -> X Bool
isFloat w = withWindowSet (pure . member w . W.floating)

showDesktopEventHook :: Event -> X All
showDesktopEventHook = \case
  ClientMessageEvent{ev_message_type} -> do
    x <- getAtom "_NET_SHOWING_DESKTOP"
    when (x == ev_message_type) showDesktop
    pure $ All True
  _ ->
    pure $ All True

clearlooks :: Theme
clearlooks = (theme wfarrTheme)
  { windowTitleIcons    = windowTitleIcons defaultThemeWithImageButtons }

clearlooks' :: Theme
clearlooks' = (theme wfarrTheme)
  { activeBorderColor   = "#5E81AC"
  , activeColor         = "#5E81AC"
  , inactiveBorderColor = "#1E3440"
  , inactiveColor       = "#1E3440"
  , activeBorderWidth   = 20
  , windowTitleIcons    = windowTitleIcons defaultThemeWithImageButtons
  }

blueBorder :: Theme
blueBorder = def
  { activeBorderColor   = "#5E81AC"
  , activeColor         = "#5E81AC"
  , inactiveBorderColor = "#1E3440"
  , inactiveColor       = "#1E3440"
  , activeBorderWidth   = 7
  }

myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [ ((modm, xK_n)                  , spawn $ XMonad.terminal conf)
       , ((modm .|. shiftMask, xK_n)    , spawn $ XMonad.terminal conf <> " -w $(xcwd)")
       , ((modm, xK_o)                  , withWindowSet dwmZero)
       , ((modm, xK_p)                  , spawn "dmenu_run")
       , ((modm .|. shiftMask, xK_p)    , dmenuFloat )
       , ((modm, xK_c)                  , spawn "clipmenu")
       , ((modm, xK_u)                  , spawn "firefox-bin")
       , ((modm .|. shiftMask, xK_c)    , kill)
       , ((modm, xK_space)              , sendMessage NextLayout)
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ((modm .|. shiftMask, xK_r)    , refresh)
       , ((modm, xK_j)                  , focusDown)
       , ((modm, xK_k)                  , focusUp)
       , ((modm, xK_BackSpace)          , focusMaster)
       , ((modm, xK_m)                  , withFocused minimizeWindow)
       , ((modm .|. shiftMask, xK_m)    , withLastMinimized maximizeWindowAndFocus)
       , ((modm, xK_f)                  , withFocused (sendMessage . maximizeRestore))
       , ((modm, xK_s)                  , showDesktop)
       , ((modm .|. shiftMask, xK_s)    , showWindows)
       , ((modm, xK_Return)             , windows W.swapMaster)
       , ((modm .|. shiftMask, xK_j)    , windows W.swapDown)
       , ((modm .|. shiftMask, xK_k)    , windows W.swapUp)
       , ((modm, xK_h)                  , sendMessage Shrink)
       , ((modm, xK_l)                  , sendMessage Expand)
       , ((modm .|. shiftMask, xK_h)    , sendMessage MirrorExpand)
       , ((modm .|. shiftMask, xK_l)    , sendMessage MirrorShrink)
       , ((modm, xK_t)                  , withFocused $ windows . W.sink)
       , ((modm, xK_d)                  , sendMessage (IncMasterN 1))
       , ((modm, xK_i)                  , sendMessage (IncMasterN (-1)))
       , ((modm, xK_b)                  , sendMessage ToggleStruts)
       , ((modm, xK_q)                  , spawn "xmonad --restart")
       , ((modm, xK_r)                  , spawn "xmonad --recompile && xmonad --restart")
       , ((modm .|. shiftMask, xK_o)    , restart "/home/fm/.local/bin/obtoxmd" True)
       , ((modm, xK_Tab)                , toggleWS)
       , ((modm .|. shiftMask, xK_q)    , spawn "lxqt-leave")
       , ((modm, xK_g     )             , gotoMenuConfig winBringer)
       , ((modm .|. shiftMask, xK_g)    , bringMenuConfig winBringer)
       ]

    ++ [((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(lazyView, 0), (W.shift, shiftMask)]]

dmenuFloat :: X ()
dmenuFloat = spawnFloat
  =<< runProcessWithInput "dmenu" []
  =<< runProcessWithInput "dmenu_path" [] []

spawnFloat :: String -> X ()
spawnFloat = spawnAndDo (insertPosition Master Newer <> doCenterFloat)


winBringer :: WindowBringerConfig
winBringer = def { windowTitler = const winClassName}

winClassName :: Window -> X String
winClassName = runQuery className

showWindows :: X ()
showWindows = traverseWindows maximizeWindow

showDesktop :: X ()
showDesktop = traverseWindows minimizeWindow

traverseWindows :: (Window -> X a) -> X ()
traverseWindows f = withWindowSet
                  $ traverse_ f
                  . W.allWindows

dwmZero :: WindowSet -> X ()
dwmZero s = toggle $ filter (== fromJust (peek s)) (W.allWindows s :: [Window])
 where
  toggle xs | length xs > 1 = killAllOtherCopies
            | otherwise     = windows copyToAll

isVisible w ws = any ((w ==) .  W.tag . W.workspace) (W.visible ws)
lazyView w ws | isVisible w ws = ws
              | otherwise      = W.view w ws
