import XMonad
import Data.Monoid

import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutHints

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)






-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "Terminal"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask -- Win / Super_L

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False


-- Width of the window border in pixels.
--
myBorderWidth = 1

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
myWorkspaces = ["tty", "web", "code" ] ++ map show [4..7] ++ ["audio", "skype"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"



------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = tiled ||| Mirror tiled ||| Full
--  where
--    -- default tiling algorithm partitions the screen into two panes
--    tiled   = Tall nmaster delta ratio
-- 
--  -- The default number of windows in the master pane
--    nmaster = 1
-- 
--    -- Default proportion of screen occupied by master pane
--    ratio   = 1/2
-- 
--    -- Percent of screen to increment by when resizing panes
--    delta   = 3/100
--
myLayoutHook = layoutHints $ avoidStruts $ toggleLayouts (noBorders Full) $ layoutHook defaultConfig


------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
--myManageHook = composeAll
--    [ className =? "MPlayer"        --> doFloat
--    , className =? "Gimp"           --> doFloat
--    , resource  =? "desktop_window" --> doIgnore
--    , resource  =? "kdesktop"       --> doIgnore ]
--
myManageHook = manageDocks <+> manageHook defaultConfig


------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
-- myLogHook = return ()
--
myLogHook xmobar = dynamicLogWithPP $ defaultPP
                   { ppOutput = hPutStrLn xmobar
                   , ppTitle = xmobarColor "white" "" . pad . shorten 125
                   , ppHiddenNoWindows = xmobarColor "#444444" "" . pad
                   , ppHidden = pad
                   , ppSep = ""
                   , ppWsSep = ""
                   , ppLayout = pad . \x -> case x of
                                              "Tall"        -> "[|]"
                                              "Mirror Tall" -> "[-]"
                                              "Full"        -> "[ ]"
                                              _             -> "[?]"
                   }




------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = return ()



main = do
--  xmobarTop <- spawnPipe "xmobar ~/.bottom_xmobarrc"
  xmobar <- spawnPipe "xmobar ~/.top_xmobarrc"
  xmonad $ defaultConfig 
             { 
               terminal = myTerminal
             , focusFollowsMouse = myFocusFollowsMouse
             , borderWidth = myBorderWidth
             , modMask = myModMask

             , workspaces = myWorkspaces  
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor


 
             -- key bindings
--             , keys               = myKeys
--             , mouseBindings      = myMouseBindings
       

             -- hooks, layouts
             , layoutHook = myLayoutHook
             , manageHook = myManageHook
             , handleEventHook    = myEventHook
             , logHook            = myLogHook xmobar
             , startupHook        = myStartupHook

             } 
             `additionalKeysP`
             [ ("<XF86AudioMute>",        spawn "amixer -q set Master     toggle")
             , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 10- unmute")
             , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 10+ unmute")
             -- Toggle fullscreen mode
             , ("<F11>", sendMessage ToggleStruts >> sendMessage ToggleLayout)
             -- Case insensitive dmenu at the bottom of the screen
             , ("M-p", spawn "exe=`dmenu_path | dmenu -b -i` && exec $exe")
             ]

