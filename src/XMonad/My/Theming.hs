module XMonad.My.Theming where

-- all the theming stuff for xmonad

import XMonad.Util.XResources

normalBG, focusBG, urgentBG :: String
normalBG = background xresources
focusBG = "#ffff5f" -- TODO change this in Xresources ?
urgentBG = color1 xresources
normalFG, focusFG, urgentFG :: String
normalFG = foreground xresources
focusFG = color11 xresources
urgentFG = color15 xresources

xftFont :: String
-- xftFont = "Fira Mono:size=8"
xftFont = "Overpass Mono:size=10"

xfont :: String
xfont = font xresources

normalBorderColor, focusedBorderColor :: String
normalBorderColor = normalBG
focusedBorderColor = focusBG
