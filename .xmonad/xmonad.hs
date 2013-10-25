import XMonad
import qualified XMonad.StackSet as S


import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect             (reflectHoriz)
import XMonad.Layout.NoBorders           (noBorders)
import XMonad.Layout.Decoration
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration

-- import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.DynamicLog    hiding (xmobar)
import XMonad.Hooks.ManageDocks          (avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops     --  (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run                   (spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.NamedWindows          (getName)

import Data.Ord                          (comparing)
import Data.List                         (intercalate, sortBy, isInfixOf)
import Data.Maybe                        (isJust, catMaybes)
import Data.Ratio                        ((%))
import Data.Monoid                       (All(All), mappend)

import Codec.Binary.UTF8.String          (encodeString)
import Control.Monad                     (when, zipWithM_, liftM2)

import Codec.Binary.UTF8.String (encode)
import Data.List
import Data.Maybe
import Data.Monoid

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName
import XMonad.Util.XUtils (fi)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties (getProp32)



-- |
-- An event hook to handle applications that wish to fullscreen using the
-- _NET_WM_STATE protocol. This includes users of the gtk_window_fullscreen()
-- function, such as Totem, Evince and OpenOffice.org.
myfullscreenEventHook :: Event -> X All
myfullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 state win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      ptype = 4 -- The atom property type for changeProperty
      chWstate f = io $ changeProperty32 dpy win state ptype propModeReplace (f wstate)

  when (typ == state && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      windows $ W.sink win

  return $ All True

myfullscreenEventHook _ = return $ All True

--  xmobar0 <- xmobar 0 "%StdinReader%}{%playing%"
--             "[Run StdinReader, Run Com \"rhythmbox-client\" [\"--print-playing\"] \"playing\" 10]"

main = do
  xmobar0 <- xmobar 0 "%StdinReader%}{%date%"
             "[Run StdinReader, Run Date \"%a %b %_d, %H:%M\" \"date\" 10]"
  xmobar1 <- xmobar 1 "%StdinReader%}{%date%"
             "[Run StdinReader, Run Date \"%a %b %_d, %H:%M\" \"date\" 10]"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {
               focusFollowsMouse  = False
             , borderWidth        = 1
             , focusedBorderColor = "#386890"
             , normalBorderColor  = "#555555"
             , terminal           = "Terminal"
             , modMask            = mod4Mask
             , logHook            = takeTopFocus
             -- myLogHook [ pp { ppOutput = hPutStrLn xmobar0 }
                --                              , pp { ppOutput = hPutStrLn xmobar1 }
                --                              ]
             , workspaces         = myWorkspaces
             , layoutHook         = myLayoutHook
             , manageHook         = myManageHook
             , startupHook        = ewmhDesktopsStartup >> setWMName "LG3D"
             , handleEventHook    = myfullscreenEventHook `mappend` handleEventHook defaultConfig
             }
             `additionalKeysP`
             [ ("M-p", withDmenu "." "dmenu_path" "exec" ["-p", "'Run:'"])
             , ("M-o", withDmenu "$LIBRARY" "ls" "evince" ["-l", "75"])
             , ("<F11>", sendMessage ToggleLayout)
             , ("M-<Esc>", spawn "slock")
             , ("<XF86AudioMute>",        spawn "amixer -q set Master     toggle")
             , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
             , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
             , ("<XF86Music>", spawn "audacious")
             , ("<XF86AudioMedia>", spawn "xfce4-mixer")
             , ("<XF86Calculator>", spawn "xcalc")
             , ("<XF86HomePage>", spawn "chromium")
             , ("<XF86WWW>",       spawn "chromium")
             , ("<XF86Mail>", spawn "chromium mail.google.com")
             , ("<XF86Messenger>", spawn "skype")
             , ("<XF86LogOff>",    spawn "slock")
             , ("<XF86Sleep>",     spawn "slock")
             , ("<XF86Documents>", spawn "epsilon")
             ]
-- XF86Music
-- XF86HomePage , XF86WWW???
-- XF86Messenger

-- XF86Mail
-- XF86LogOff
-- XF86Sleep?

-- XF86Documents

-- XF86Pictures ???


-- myWorkspaces = ["1:edit", "2:surf", "3:read", "4:chat", "5:play", "6:misc", "7:misc", "8:misc", "9:misc"]
myWorkspaces = ["1:tty", "2:surf", "3:edit", "4:read", "5:misc", "6:misc", "7:misc", "8:audio", "9:chat"]

myLayoutHook = toggleLayouts (noBorders Full) defaultLayout
    where
      defaultLayout
          = avoidStruts
          $ noFrillsDeco shrinkText myTheme
          $ onWorkspace "9:chat" chatLayout
          $ layoutHook defaultConfig
      chatLayout
          = withIM (1/6) (Role "MainWindow")
          $ reflectHoriz
          $ withIM (1/5) (Role "buddy_list") Grid


myTheme = defaultTheme {
            activeColor         = "SteelBlue"
          , activeBorderColor   = "#386890"
          , inactiveBorderColor = "#555555"
          , fontName            = "xft:DejaVu Sans-10:bold"
          }


myManageHook = manageDocks <+> manageFloats <+> manageApps
    where manageFloats = composeOne [ isFullscreen -?> doFullFloat
                                    , isDialog     -?> doFloat
                                    ]
          manageApps   = composeAll [ 
                                      className =? "Terminal"         --> moveTo "1:tty"
                                    , className =? "Emacs"         --> moveTo "3:edit"
                                    , className =? "Epsilon"       --> moveTo "3:edit"
                                    , className =? "jed"           --> moveTo "3:edit"
                                    , className =? "Iceweasel"     --> moveTo "2:surf"
                                    , className =? "Google-chrome" --> moveTo "2:surf"
                                    , className =? "Chromium" --> moveTo "2:surf"
                                    , className =? "chromium" --> moveTo "2:surf"
                                    , className =? "Google-Chromium" --> moveTo "2:surf"
                                    , className =? "Google-chromium" --> moveTo "2:surf"
                                    , className =? "X-www-browser" --> moveTo "2:surf"
                                    , className =? "GV"            --> moveTo "4:read"
                                    , className =? "XDvi"          --> moveTo "4:read"
                                    , className =? "Xpdf"          --> moveTo "4:read"
                                    , className =? "Evince"        --> moveTo "4:read"
                                    , className =? "Skype"         --> moveTo "9:chat"
                                    , className =? "Pidgin"        --> moveTo "9:chat"
                                    , className =? "Rhythmbox"     --> moveTo "8:audio"
                                    , className =? "Mixer"         --> moveTo "8:audio"
                                    , className =? "xfce4-mixer"   --> moveTo "8:audio"
                                    , className =? "audacious"     --> moveTo "8:audio"
                                    , className =? "Audacious"     --> moveTo "8:audio"
                                    ]
          moveTo       = doF . liftM2 (.) S.view S.shift


xmobar screen template commands = spawnPipe . unwords $ options
    where options = [ "xmobar"
                    , "-x"
                    , show screen
                    , "-t"
                    , wrap "'" "'" template
                    , "-c"
                    , wrap "'" "'" commands
                    ]


currentScreenID :: X Int
currentScreenID = (fromIntegral . S.screen . S.current) `fmap` gets windowset


withDmenu :: String -> String -> String -> [String] -> X ()
withDmenu dir src prg opt = do
  screen <- currentScreenID
  let tmp = wrap "`" "`" . unwords $ [ src
                                     , "|"
                                     , "dmenu"
                                     , "-i"
                                     , "-xs"
                                     , show screen
                                     ] ++ opt
      cmd = unwords [ "cd"
                    , dir
                    , "&&"
                    , "tmp=" ++ tmp
                    , "&&"
                    , prg
                    , "\"$tmp\""
                    ]
  io . spawn $ cmd


pp = defaultPP {
       ppHiddenNoWindows = xmobarColor "DimGray"      ""            . pad
     , ppCurrent         = xmobarColor "White"        "#555555"     . pad
     , ppVisible         = pad
     , ppHidden          = pad
     , ppUrgent          = xmobarColor ""             "LightSalmon"       . xmobarStrip
     , ppLayout          = xmobarColor "LightSkyBlue" ""            . pad . iconify
     , ppTitle           = xmobarColor "PaleGreen"    ""            . pad . shorten 200
     , ppWsSep           = ""
     , ppSep             = ""
     , ppOrder           = \(ws:l:_:rest) -> (ws:l:rest)
     }
    where
      iconify l | "Mirror" `isInfixOf` l = "[-]"
                | "Grid"   `isInfixOf` l = "[+]"
                | "Tall"   `isInfixOf` l = "[|]"
                | "Full"   `isInfixOf` l = "[ ]"
                | otherwise              = l


myLogHook pps = do
  takeTopFocus >> setWMName "LG3D"
  screens <- (sortBy (comparing S.screen) . S.screens) `fmap` gets windowset
  zipWithM_ dynamicLogWithPP' screens pps

-- Extract the focused window from the stack of windows on the given screen.
-- Return Just that window, or Nothing for an empty stack.
focusedWindow = maybe Nothing (return . S.focus) . S.stack . S.workspace

-- The functions dynamicLogWithPP', dynamicLogString', and pprWindowSet' below
-- are similar to their undashed versions, with the difference being that the
-- latter operate on the current screen, whereas the former take the screen to
-- operate on as the first argument.

dynamicLogWithPP' screen pp = dynamicLogString' screen pp >>= io . ppOutput pp

dynamicLogString' screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort pp

  -- layout description
  let ld = description . S.layout . S.workspace $ screen

  -- workspace list
  let ws = pprWindowSet' screen sort' urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle  pp wt
             ]
             ++ catMaybes extras


pprWindowSet' screen sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces s
    where this     = S.tag . S.workspace $ screen
          visibles = map (S.tag . S.workspace) (S.current s : S.visible s)

          fmt w = printer pp (S.tag w)
              where printer | S.tag w == this                                               = ppCurrent
                            | S.tag w `elem` visibles                                       = ppVisible
                            | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (S.stack w)                                            = ppHidden
                            | otherwise                                                     = ppHiddenNoWindows


sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)


-- Send WM_TAKE_FOCUS
takeTopFocus = withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek

atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"
takeFocusX w = withWindowSet $ \ws -> do
    dpy <- asks display
    wmtakef <- atom_WM_TAKE_FOCUS
    wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $ do
        io $ allocaXEvent $ \ev -> do
            setEventType ev clientMessage
            setClientMessageEvent ev w wmprot 32 wmtakef currentTime
            sendEvent dpy w False noEventMask ev
