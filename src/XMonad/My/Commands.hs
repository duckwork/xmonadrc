module XMonad.My.Commands
    ( commands
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.My.Helpers
import XMonad.Hooks.ManageDocks
import System.Exit

commands :: X [(String, X ())]
commands = do
    wscmds <- workspaceCommands
    return $ wscmds ++ screenCommands ++ xmonadCommands

xmonadCommands :: [(String, X ())]
xmonadCommands =
    [ ("shrink", sendMessage Shrink)
    , ("expand", sendMessage Expand)
    , ("next-layout", sendMessage NextLayout)
    , ("default-layout", asks (layoutHook . config) >>= setLayout)
    , ("restart-wm", restart "xmonad" True)
    , ("restart-wm-no-resume", restart "xmonad" False)
    , ("kill", kill)
    , ("refresh", refresh)
    , ("rescreen", rescreen)
    , ("focus-up", windows W.focusUp)
    , ("focus-down", windows W.focusDown)
    , ("swap-up", windows W.swapUp)
    , ("swap-down", windows W.swapDown)
    , ("swap-master", windows W.swapMaster)
    , ("float", withFocused float)
    , ("sink", withFocused $ windows . W.sink)
    , ("quit-wm", io $ exitSuccess)
    , ("refresh-panel", runLogHook)
    , ("toggle-struts", sendMessage ToggleStruts)
    , ("unset-struts", strutsOff)
    , ("set-struts", strutsOn)
    , ("fullscreen", fullScreenFocused)
    ]

workspaceCommands :: X [(String, X ())]
workspaceCommands = 
    asks (workspaces . config) >>= \spaces -> return
        [(m ++ i, windows $ f i)
            | i <- spaces
            , (f,m) <- [(W.view, "view-"), (W.shift, "shift-")]
        ]

screenCommands :: [(String, X ())]
screenCommands =
    [ (m ++ show sc, screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
        | sc <- [0,1] :: [Int] -- TODO adapt to screen changes
        , (f, m) <- [(W.view, "screen-"), (W.shift, "screen-to-")]
    ]
