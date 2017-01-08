module Main where

import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)

import qualified XMonad.My.Hooks as My
import qualified XMonad.My.Keys as My
import qualified XMonad.My.Layout as My
import qualified XMonad.My.Theme as My
import qualified XMonad.My.Workspaces as My

main :: IO ()
main
  = xmonad $ ewmh def
    { modMask                   = mod4Mask
    , terminal                  = "terminal"
    , borderWidth               = 2
    , normalBorderColor         = My.normalBorderColor
    , focusedBorderColor        = My.focusedBorderColor
    , clickJustFocuses          = False
    , focusFollowsMouse         = True
    , workspaces                = My.workspaces
    -- , floatFocusFollowsMouse    = True
    -- , focusRaisesFloat          = True
    -----------------------------
    , handleEventHook           = My.handleEventHook
    , layoutHook                = My.layoutHook
    , logHook                   = My.logHook
    , manageHook                = My.manageHook
    , startupHook               = My.startupHook
    -- , floatHook                 = My.floatHook
    -----------------------------
    , keys                      = My.keys
    , mouseBindings             = My.mouseBindings
    }
