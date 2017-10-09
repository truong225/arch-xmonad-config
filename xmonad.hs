-- import XMonad
-- import XMonad.Util.Run(spawnPipe)
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Util.EZConfig(additionalKeys)
-- import System.IO

-- main= doq
--     xmproc <-spawnPipe "xmobar" 
--     xmonad $ def
--         { modMask=mod4Mask
--         , terminal="urxvt"
--         , borderWidth=2
--         } `additionalKeys`
--         [((mod4Mask .|. shiftMask, xK_d), spawn "rofi -show run")   --Set key Mod+Shift+D
--         ,((mod4Mask, xK_s), spawn "conky")
--         ]

import XMonad
import Control.Monad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Config as DefConfig
import GHC.IO.Handle.Types as H

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO


---------------------------------------------------------------------

-- Color shortcut --
black = "#333333"
grey1 = "#BFBFBF"
grey2 = "#7B7B7B"
grey3 = "#EEEEEE"
grey5 = "#DCDCDC"
grey7 = "#194350"
white = "#FFFFFF"
blue1 = "#60B0D2"
blue2 = "#5C86FF"
blue3 = "#30B1B8"
pink1 = "#E38179"
navy1 = "#02151B"
navy2 = "#46B0B3"
navy3 = "#317876"
redd1 = "#B84130"
bgColor = "#EEEEEE"



-- Call shortcut for .xbm links
i = " ^i(/dzen2/"


---------------------------------------------------------------------


myws :: [String]
myws = clickable $ [i++"term.xbm) TERM"
                    , i++"cat.xbm) WEB"
                    , i++"eye_r.xbm) INFO"
                    , i++"pacman.xbm) FILE"
                    , i++"diskette.xbm) WORK"
                    ]
        where clickable l = ["^ca(1,xdotool key super+" ++ show(n)  ++ ")" ++ ws ++
                        "^ca()" | (i,ws) <- zip [1..] l, let n = i 
                        ]


-- Make key binding
-- Call mkeys
mkeys=[((mod4Mask, xK_Return), spawn "urxvt -fn 'xft:dejavu:pixelsize=10'")
        -- Function keys
        , ((mod4Mask, xK_Left), prevWS)
        , ((mod4Mask, xK_Right), nextWS)
        , ((mod4Mask, xK_Up), sendMessage MirrorExpand)
        , ((mod4Mask, xK_Down), sendMessage MirrorShrink)
        , ((mod4Mask, xK_r), spawn "rofi -show run")
        , ((mod4Mask .|. shiftMask, xK_r), spawn "killall dzen2 && killall conky && xmonad --restart")
        , ((mod4Mask .|. controlMask, xK_s), sendMessage Arrange)

        -- Sound and Others
        -- , ((mod4Mask .|. mod1Mask, xK_Up), spawn "amixer set Master 5%+")
        -- , ((mod4Mask .|. mod1Mask, xK_Down), spawn "amixer set Master 5%-")
        
        -- , ((0, xK_F9), spawn "mpc toggle")
        -- , ((0, xK_F11), spawn "mpc prev")
        -- , ((0, xK_F12), spawn "mpc next")
    ]



logBar h = do
    dynamicLogWithPP $ tryPP h
tryPP :: Handle -> PP
tryPP h = XMonad.Hooks.DynamicLog.def
        { ppOutput          = hPutStrLn h
        , ppCurrent         = dzenColor (white) (pink1) . pad
        , ppVisible         = dzenColor (black) (grey3) . pad
        , ppHidden          = dzenColor (black) (grey3) . pad
        , ppHiddenNoWindows = dzenColor (grey1) (grey3) . pad
        , ppUrgent          = dzenColor (blue1) (grey3) . pad
        , ppWsSep           = ""
        , ppOrder           = \(ws:l:t:_) -> [ws]
        , ppSep             = " ^r(4x4) "
        , ppLayout          = dzenColor (black) (navy3) . ( \t -> case t of
            "Spacing 2 Tall" -> " " ++ k ++ "tile.xbm) tile 4" )
        }
        where k = "^i(/xbm/file_tile/"

-- Main Class --
myfn= "xft:monospace:pixelsize=10:bold"  --"M+ lm-9:Bold" -- Set font for the panel
-- Layout --
res=ResizableTall 1 (2/100) (1/2) [] -- Part of layouts --
ful=noBorders (fullscreenFull Full)

start :: X()
start = do
        spawnOnce "compton -f -D8 -IO.05 -O0.05"
        spawnOnce "feh  --bg-scale --no-fehbg ~/.xmonad/.wallpaper/xmonad-circle.png"
        -- "feh --randomize --no-fehbg --bg-scale ~/.wallpaper/*"

-- For the main class --
main = do
        panel <- spawnPipe top  -- This is the panel --
        panel2 <- spawnPipe "sh /script/katana.sh"
        
        xmonad $ def
            {manageHook = manageDocks <+> manageHook DefConfig.def
            , layoutHook = avoidStruts (spacing 2 $ layoutHook
                DefConfig.def ||| res
            )
                ||| ful

            , modMask = mod4Mask
            , focusedBorderColor = "#B84130"
            , normalBorderColor = "#02151B"
            , borderWidth = 3
            , workspaces = myws
            , terminal = "urxvt"
            , startupHook = start
            , logHook = logBar panel
            } `additionalKeys` mkeys
            where top = "dzen2 -p -ta 'l' -e 'button3=' -fn '"
                        ++ myfn ++ "' -fg '" ++ navy1 ++ "' -bg '" ++ bgColor
                        ++ "' -w 500"
                        ++ " -h 22 "












