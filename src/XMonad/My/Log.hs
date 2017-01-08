module XMonad.My.Log where

import Data.List (intersperse)
import GHC.IO.Handle (Handle(..))
import XMonad
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)
import XMonad.Hooks.DynamicLog
import XMonad.My.Theming
import XMonad.Util.Lemonbar
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn)

panel :: Maybe Handle -> PP
panel mh = def
    { ppCurrent -- current workspace
      = \ws -> 
        lbColor "" focusBG
        . lbClick 1 ("xmonadctl view-"++ws)
        . space $ ws
    , ppVisible -- visible workspace
      = \ws ->
        normalOpaque . lbUnderline
        . lbClick 1 ("xmonadctl view-"++ws)
        . space $ ws
    , ppHidden -- hidden workspace with windows
      = \ws ->
        normalOpaque
        . lbClick 1 ("xmonadctl view-"++ws)
        . space $ ws
    , ppHiddenNoWindows -- hidden workspace w/o windows
      = const ""
    , ppUrgent -- workspace with urgent window
      = lbColor urgentFG urgentBG . space
    , ppSort -- how to sort workspaces
      = getSortByOrder
    , ppWsSep -- separator b/w workspaces
      = ""
    ----
    , ppTitle -- the current window's title
      = normalOpaque . sep' . space . shorten 20
    , ppLayout -- the current layout's name
      = normalOpaque . sep' . lbClick 1 "xmonadctl next-layout" . space
    ----
    , ppSep -- separator b/w sections
      = ""
    , ppOrder -- the order of sections
      = \(ws:l:t:xs) ->
      [ lbAlignLeft, ws
      , l
      , t
      , lbAlignRight, concat xs
      ]
    , ppOutput -- where to output the formatted string
      = maybe (\s -> return ()) (hPutStrLn) mh
    , ppExtras -- loggers and other goodies
      = loggers
    } where
        space = wrap " " " "
        sep' = wrap (lbColor "#aaaaaa" "" sep) ""
        normalOpaque = lbColor "" normalBG

loggers :: [Logger]
loggers = reverse
    [ normalOpaqueL $ dateLog
    , normalOpaqueL $ batLog
    , normalOpaqueL $ volumeLog
    , normalOpaqueL $ wifiLog
    , normalOpaqueL $ mpdLog
    ] where
        dateLog = wrapL "" " " $ date "%H:%M %a %e"
        batLog = wrapL "" (sepClr sep') 
            $ logCmd "batstat"
        volumeLog
          = wrapL "" (sepClr sep')
          . lbClickL 3 "volstat display"
          . lbClickL 1 "volstat toggle"
          $ logCmd "volstat"
        wifiLog = wrapL " " (sepClr sep') 
            $ logCmd "wifistat"
        mpdLog 
          = wrapL "" (sepClr $ " "++sep)
          . lbClickL 1 "urxvtc -n ncmpcpp -e ncmpcpp"
          . shortenL 25
          $ logCmd "mpdstat"
        normalOpaqueL = lbColorL "" normalBG
        sepClr = lbColor "#aaaaaa" normalBG
        sep' = wrap " " " " sep

sep :: String
-- sep = "│"
sep = "¦"
