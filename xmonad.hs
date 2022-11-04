{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad                       (when)
import           Data.Foldable                       (traverse_)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust, isNothing)
import           Data.Monoid                         (All (All))
import           IfMaxAlt
import           Prelude
import           XMonad
import           XMonad.Actions.CopyWindow           (copyToAll,
                                                      killAllOtherCopies)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Minimize             (maximizeWindow,
                                                      maximizeWindowAndFocus,
                                                      minimizeWindow,
                                                      withLastMinimized)
import           XMonad.Actions.WindowBringer        (WindowBringerConfig (windowTitler),
                                                      bringMenu,
                                                      bringMenuConfig, gotoMenu,
                                                      gotoMenuConfig)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicProperty        (dynamicPropertyChange)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (Newer),
                                                      Position (End),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts, docks)
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows         (boringWindows, focusDown,
                                                      focusMaster, focusUp)
import           XMonad.Layout.ImageButtonDecoration (defaultThemeWithImageButtons)
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize              (minimize)
import           XMonad.Layout.ResizableTile         (MirrorResize (..),
                                                      ResizableTall (..))
import           XMonad.Layout.SideBorderDecoration
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.Simplest              (Simplest (Simplest))
import qualified XMonad.StackSet                     as W
import           XMonad.StackSet                     (integrate', peek, stack)
import           XMonad.Util.Cursor
import           XMonad.Util.NamedWindows            (getNameWMClass)
import           XMonad.Util.Themes
import           XMonad.Util.WorkspaceCompare        (WorkspaceSort,
                                                      filterOutWs)
main = xmonad
     . ewmhFullscreen
     . addEwmhWorkspaceSort (filterEmpty <$> gets windowset)
     . ewmh
     . docks
     $ conf

conf = desktopConfig
     { terminal           = "alacritty"
     , modMask            = mod4Mask
     , startupHook        = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
     , keys               = myKeys
     , normalBorderColor  = "#2E3440"
     , focusedBorderColor = "#5E81AC"
     , borderWidth        = 0
     , layoutHook         = layoutHook'
     , manageHook         = manageHook'
     , handleEventHook    = handleEventHook'
     , focusFollowsMouse  = False
     }

layoutHook' =
  let
    tiled = ResizableTall 1 (1 / 20) (103 / 200) []
    decor (x :: l Window) = sideBorderLayout (sideBorderConf conf) x
    -- decor = imageButtonDeco clearlooks
  in
    avoidStruts
  . maximizeWithPadding 0
  . minimize
  . boringWindows
  . ifMax 1 Simplest
  . decor
  $ tiled ||| Full ||| Mirror tiled

handleEventHook' :: Event -> X All
handleEventHook' = composeAll
  [ minimizeEventHook
  , showDesktopEventHook
  , dynamicPropertyChange "WM_CLASS" (className =? "Spotify" --> doShift "9")
  ]

manageHook' :: ManageHook
manageHook' = composeAll
  [ placeHook $ smart (0.5, 0.5)
  , className =? "discord" --> doShift "9"
  , className =? "Thunderbird" --> doShift "8"
  , className =? "qBittorrent" --> doShift "7"
  , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
  , insertPosition End Newer
  ]

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

showDesktopEventHook :: Event -> X All
showDesktopEventHook = \case
  ClientMessageEvent{ev_message_type} -> do
    x <- getAtom "_NET_SHOWING_DESKTOP"
    when (x == ev_message_type) showDesktop
    pure $ All True
  _ ->
    pure $ All True

sideBorderConf :: XConfig a -> SideBorderConfig
sideBorderConf conf = def
  { sbSide = U
  , sbActiveColor = XMonad.focusedBorderColor conf
  , sbInactiveColor = XMonad.normalBorderColor conf
  , sbSize = 7
  }

clearlooks :: Theme
clearlooks = (theme wfarrTheme)
  { windowTitleIcons = windowTitleIcons defaultThemeWithImageButtons }

myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [ ((modm, xK_n)                  , spawn $ XMonad.terminal conf)
       , ((modm .|. shiftMask, xK_n)    , spawn $ XMonad.terminal conf <> " --working-directory $(xcwd)")
       , ((modm, xK_o)                  , withWindowSet dwmZero)
       , ((modm, xK_p)                  , spawn "dmenu_run")
       , ((modm, xK_c)                  , spawn "clipmenu")
       , ((modm, xK_u)                  , spawn "firefox")
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

winBringer :: WindowBringerConfig
winBringer = def { windowTitler = const winClassName}

winClassName :: Window -> X String
winClassName = fmap show . getNameWMClass

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
