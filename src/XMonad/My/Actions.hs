module XMonad.My.Actions
    ( handleEventHook
    , manageHook
    , startupHook
    , logHook
    ) where

import Data.List (intersperse)
import Data.Monoid
import XMonad hiding (handleEventHook, manageHook, startupHook, logHook)
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ToggleFade
import XMonad.My.Commands
import XMonad.My.Helpers
import XMonad.My.Preferences
import XMonad.Util.SpawnNamedPipe (getNamedPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.My.Log as My
import qualified XMonad.StackSet as W

handleEventHook :: Event -> X All
handleEventHook = composeAll
    [ docksEventHook
    , serverModeEventHookCmd' commands
    , fullscreenEventHook
    , positionStoreEventHook
    ]

manageHook :: ManageHook
manageHook = composeAll
    [ manageDocks
    , placeHook placement
    , positionStoreManageHook Nothing
    , composeOne
        [ isFullscreen -?> doFullFloat
        -- , title =? "xpanel" -?> doF W.swapDown
        -- Force dialog windows and pop-ups to be floating.
        , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
        , stringProperty "WM_WINDOW_ROLE" =? gtkFile -?> doCenterFloat
        , isDialog -?> doCenterFloat
        , className =? "Xmessage" -?> doCenterFloat
        , transience -- Move transient windows to their parent.
        -- Don't steal master
        , className =? "URxvt" -?> tileBelow
        -- everything else
        , pure True -?> normalTile
        ]
    ]
  where
    gtkFile = "GtkFileChooserDialog"
    normalTile = insertPosition Above Newer
    tileBelow = insertPosition Below Newer

logHook :: X ()
logHook = do
    historyHook
    toggleFadeLogHook 0.8
    h <- getNamedPipe "xpanel"
    dynamicLogWithPP $ My.panel h
    return ()

startupHook :: X ()
startupHook = do 
    setFullscreenSupported
    lemonbar
    spawnOnce "xmonad-ticker.sh"
