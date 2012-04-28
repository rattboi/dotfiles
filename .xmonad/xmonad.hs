import XMonad hiding ((|||))
 
import System.Exit
import System.IO
import Data.Ratio ((%))

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as S
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS

myModMask = mod4Mask
altMask = mod1Mask

workspaces' :: [WorkspaceId]
workspaces' = ["1-term","2-web","3-ssh","4-email","5-skype","6-scratch"]


myKeys =
	[ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
	, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	, ((0, xK_Print), spawn "scrot")
	, ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
	, ((0, 0x1008ff11), spawn "amixer -q set Master 2- unmute")
	, ((0, 0x1008ff13), spawn "amixer -q set Master 2+ unmute")
	, ((0, 0x1008ff14), spawn "mpc toggle")
	, ((0, 0x1008ff16), spawn "mpc prev")
	, ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff81), spawn "mpc random")
	, ((altMask, xK_Tab ), windows S.focusDown)
	, ((altMask .|. shiftMask, xK_Tab ), windows S.focusUp)
	, ((altMask .|. controlMask, xK_p ), runOrRaisePrompt defaultXPConfig)
    , ((myModMask, xK_u), focusUrgent)
    , ((myModMask, xK_t), spawn "thunar")
    , ((myModMask .|. altMask, xK_l), spawn "xlock")
    , ((myModMask, xK_Right), nextWS)
    , ((myModMask, xK_Left),  prevWS)
    , ((myModMask .|. shiftMask, xK_r), spawn "~/.bin/rotate.sh")
    , ((myModMask .|. shiftMask, xK_t), spawn "~/.bin/trackpad-toggle.sh")
	]

myManageHook = (composeAll . concat $
	[ [ isFullscreen -->  doFullFloat ] ,  
	  [ className =? c --> doFloat | c <- floats] ,
	  [ className =? w --> doF ( S.shift "2-web") | w <- webs] ,
	  [ resource  =? "desktop_window" --> doIgnore
	  , className =? "urxvt"          --> doShift "1-term"
	  , className =? "Skype"          --> doShift "5-skype"
          , title     =? "ssh"            --> doShift "3-ssh"
	  ] ])
	  where floats = ["MPlayer", ".", "feh"]
	        webs   = ["Firefox-bin", "Firefox", "Minefield"]

imLayout = avoidStruts $
	   IM (1%7) (Or (And (ClassName "Pidgin") (Role "buddy_list"))
	                (And (ClassName "Skype") (Role "MainWindow")))

tabbedLayout = tabbed shrinkText tabbedConf

tabbedConf = defaultTheme {
   fontName = "xft:Terminus"
}

genericLayouts = avoidStruts $ smartBorders $ toggleLayouts (noBorders Full) $
		 tiled ||| Mirror tiled||| tabbedLayout  ||| (noBorders Full)
	where
		tiled = Tall 1 (3 / 100) (1 / 2)

myLayouts = onWorkspace "5-skype" (imLayout) $
	    genericLayouts


main = do
	xmproc <- spawnPipe "xmobar ~/.xmobarrc"
	xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
		{ manageHook = myManageHook <+> manageDocks
        , startupHook = setWMName "LG3D"
		, workspaces = workspaces'
        , focusFollowsMouse = False
		, layoutHook = myLayouts
		, borderWidth = 2
		, terminal = "urxvt"
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 50
			} 
		, modMask = myModMask
		} `additionalKeys` myKeys
