module XMonad.My.Theme 
    ( normalBG, focusBG, urgentBG
    , normalFG, focusFG, urgentFG
    , xfont, lemonFont
    , normalBorderColor, focusedBorderColor
    , decTheme, promptTheme
    , module XMonad.Util.XResources
    ) where

-- all the theming stuff for xmonad

import Data.List
import XMonad.Float.SimplestFloatDec
import qualified XMonad.Prompt as P
import XMonad.Util.XResources

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

lemonFont :: String
-- lemonFont = "Fixedsys Excelsior 3.01"
lemonFont = "Roboto Mono:size=8"

normalBorderColor, focusedBorderColor :: String
normalBorderColor = normalBG
focusedBorderColor = normalFG

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


promptTheme :: P.XPConfig
promptTheme = def
    { P.font = "xft:" ++ lemonFont
    , P.bgColor = normalBG
    , P.fgColor = normalFG
    , P.fgHLight = normalBG
    , P.bgHLight = normalFG
    , P.borderColor = normalBG
    , P.promptBorderWidth = 1
    , P.position = P.Top
    , P.alwaysHighlight = True
    , P.height = 20
    , P.maxComplRows = Nothing
    , P.historySize = 100
    , P.searchPredicate = isInfixOf
    }
