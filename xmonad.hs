{-# LANGUAGE NamedFieldPuns #-}
import           Control.Monad                    (when)
import           Data.Foldable                    (traverse_)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      (All (All))
import           IfMaxAlt
import           Prelude
import           XMonad
import           XMonad.Actions.CopyWindow        (copyToAll,
                                                   killAllOtherCopies)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Minimize          (maximizeWindow,
                                                   maximizeWindowAndFocus,
                                                   minimizeWindow,
                                                   withLastMinimized)
import           XMonad.Config.Desktop
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition      (Focus (Newer),
                                                   Position (End),
                                                   insertPosition)
import           XMonad.Hooks.ManageDocks         (ToggleStruts (ToggleStruts),
                                                   avoidStruts, docks)
import           XMonad.Hooks.Minimize            (minimizeEventHook)
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows      (BoringWindows, boringWindows,
                                                   focusDown, focusMaster,
                                                   focusUp)
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize           (minimize)
import           XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import           XMonad.Layout.ResizableTile      (MirrorResize (..),
                                                   ResizableTall (..))
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.Simplest           (Simplest (Simplest))
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet                  as W
import           XMonad.StackSet                  (integrate', peek, stack)
import           XMonad.Util.Cursor
import           XMonad.Util.Themes

main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     $ desktopConfig

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
    decor = buttonDeco shrinkText defaultThemeWithButtons
    -- decor = simpleDeco shrinkText (theme wfarrTheme)
  in
    avoidStruts
  . maximizeWithPadding 0
  . minimize
  . boringWindows
--  . spacingRaw True (Border 0 0 0 0) False (Border 0 0 8 0) True
  . ifMax 1 Simplest
  $ decor tiled ||| Full ||| decor (Mirror tiled)

handleEventHook' :: Event -> X All
handleEventHook' = composeAll
  [ minimizeEventHook
  , showDesktopEventHook
  ]


manageHook' :: ManageHook
manageHook' = composeAll
  [ placeHook $ smart (0.5, 0.5)
  , className =? "Animator" --> doFloat
  , insertPosition End Newer
  ]

showDesktopEventHook :: Event -> X All
showDesktopEventHook ClientMessageEvent{ev_message_type} = do
  x <- getAtom "_NET_SHOWING_DESKTOP"
  when (x == ev_message_type) showDesktop
  return (All True)

myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [ ((modm, xK_n)                  , spawn $ XMonad.terminal conf)
       , ((modm, xK_o)                  , withWindowSet dwmZero)
       , ((modm, xK_p)                  , spawn "dmenu_run")
       , ((modm, xK_c)                  , spawn "clipmenu")
       , ((modm, xK_u)                  , spawn "chromium")
       , ((modm .|. shiftMask, xK_c)    , kill)
       , ((modm, xK_space)              , sendMessage NextLayout)
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ((modm .|. shiftMask, xK_n)    , refresh)
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
       ]

    ++ [((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(lazyView, 0), (W.shift, shiftMask)]]


showWindows :: X ()
showWindows = traverseWindows maximizeWindow

showDesktop :: X ()
showDesktop = traverseWindows minimizeWindow

traverseWindows :: (Window -> X a) -> X ()
traverseWindows f = withWindowSet
                  $ traverse_ f
                  . concatMap (integrate' . stack)
                  . W.workspaces


dwmZero :: WindowSet -> X ()
dwmZero w = toggle $ filter (== (fromJust . peek) w) ws
 where
  toggle xs | length xs > 1 = killAllOtherCopies
            | otherwise     = windows copyToAll

  ws = concatMap (integrate' . stack) $ W.workspaces w


isVisible w ws = any ((w ==) .  W.tag . W.workspace) (W.visible ws)
lazyView w ws | isVisible w ws = ws
              | otherwise      = W.view w ws


{-
greedyView :: (Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
greedyView w ws
     | any wTag (W.hidden ws) = W.view w ws
     | (Just s) <- find (wTag . W.workspace) (W.visible ws)
                            = ws { W.current = (W.current ws) { W.workspace = W.workspace s }
                                 , W.visible = s { W.workspace = W.workspace (W.current ws) }
                                           : filter (not . wTag . W.workspace) (W.visible ws) }
     | otherwise = ws
   where wTag = (w == ) . W.tag


addWorkspaceAt :: (WindowSpace -> [WindowSpace] -> [WindowSpace]) -> String -> X ()
addWorkspaceAt add newtag = addHiddenWorkspaceAt add newtag >> windows (greedyView newtag)
-}



