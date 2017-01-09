module XMonad.My.Theme where

-- all the theming stuff for xmonad

import XMonad.Util.XResources
import XMonad.Float.SimplestFloatDec

normalBG, focusBG, urgentBG :: String
normalBG = background xresources
focusBG = "#ffff5f" -- TODO change this in Xresources ?
urgentBG = color1 xresources
normalFG, focusFG, urgentFG :: String
normalFG = foreground xresources
focusFG = color11 xresources
urgentFG = color15 xresources

xfont :: String
xfont = font xresources

normalBorderColor, focusedBorderColor :: String
normalBorderColor = normalBG
focusedBorderColor = focusBG

decTheme :: Theme
decTheme = def
    { activeColor = focusBG
    , inactiveColor = normalBG
    , urgentColor = urgentBG
    , activeBorderColor = focusBG
    , inactiveBorderColor = normalBG
    , urgentBorderColor = urgentBG
    , activeTextColor = focusFG
    , inactiveTextColor = normalFG
    , urgentTextColor = urgentFG
    , fontName = xfont
    , decoHeight = 14
    }

