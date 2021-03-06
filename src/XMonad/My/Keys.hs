{-# OPTIONS -fno-warn-type-defaults #-}
module XMonad.My.Keys
    ( keys
    , rawKeys
    , mouseBindings
    ) where

import System.Exit
import XMonad hiding (keys, mouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Promote
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ToggleFade
import XMonad.Layout.MouseResizableTile (MRTMessage(..))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLayouts hiding (Toggle)
import XMonad.My.Commands
import XMonad.My.Helpers
import XMonad.My.Theme
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W

import XMonad.Prompt.RunOrRaise

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XMonad.modMask = mm} = M.fromList
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w)
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((mm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    ]

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
    keymaps =
        [ xmonadKeys
        , clientKeys
        , workspaceKeys
        , screenKeys
        , appKeys
        , layoutKeys
        , fnKeys
        ]

xmonadKeys :: XConfig Layout -> [(String, X ())]
xmonadKeys _ =
    [ ("M-S-q", io (exitSuccess))
    , ("M-S-r", spawn restarter)
    , ("M-a", commands >>= 
              \c -> xmonadPromptC c promptTheme)
    ]
  where
    restarter = "xmonad --recompile && xmonad --restart"

clientKeys :: XConfig Layout -> [(String, X ())]
clientKeys _ =
    [ ("M-q", kill)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-`", withFocused toggleFloat)
    , ("M-\\", withFocused toggleFade)
    , ("M1-<Tab>", nextMatch Forward (return True))
    ]

workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys _ 
  = zip (map ((++) "M-") (map show [1..9])) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (map ((++) "M-S-") (map show [1..9])) (map (withNthWorkspace W.shift) [0..])
    ++
    [ (m ++ "M-" ++ k, f d NonEmptyWS)
    | (k, d) <- [ (",", Prev), (".", Next) ]
    , (m, f) <- [ ("", DO.moveTo)
                , ("S-", \d' t -> DO.shiftTo d' t >> DO.moveTo d' t)
                , ("C-", DO.swapWith)
                ]
    ] ++
    [ ("M-<Tab>", toggleWS)
    ]

screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
    [ ("M-" ++ mask ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
        | (key, scr) <- zip "wer" [0..]
        , (mask, action) <- [ ("", W.view)
                            , ("S-", W.shift)
                            ]
    ]

appKeys :: XConfig Layout -> [(String, X ())]
appKeys c =
    [ ("M-;", runOrRaisePrompt promptTheme)
    , ("M-p", spawn "dmenu_run -y -1 -h 20")
    , ("M-<Return>", spawn $ terminal c)
    ]

layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys c =
    [ ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", setLayout $ layoutHook c)
    , ("M-m", windows W.focusMaster)
    , ("M-S-m", promote)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-o", sendMessage ExpandSlave)
    , ("M-i", sendMessage ShrinkSlave)
    , ("M-S-h", sendMessage $ IncMasterN 1)
    , ("M-S-l", sendMessage $ IncMasterN (-1))
    ---------
    , ("M-x", sendMessage ToggleLayout)
    , ("M-f", sendMessage $ Toggle NBFULL)
    , ("M-b", sendMessage $ ToggleStruts)
    ---------
    , ("M-S-`", toggleFloatAll)
    ]

-- toggleStrutsHack :: X ()
-- toggleStrutsHack = spawn "toggle-lb-hack.sh"

fnKeys :: XConfig Layout -> [(String, X ())]
fnKeys _ = 
    [ ("<XF86AudioLowerVolume>", logspawn "er soft")
    , ("<XF86AudioRaiseVolume>", logspawn "er loud")
    , ("<XF86AudioMute>", logspawn "er mute")
    , ("<XF86MonBrightnessUp>", logspawn "er bright")
    , ("<XF86MonBrightnessDown>", logspawn "er dim")
    , ("<XF86AudioPlay>", logspawn "mpc toggle")
    , ("<XF86AudioStop>", logspawn "mpc pause")
    , ("<XF86AudioNext>", logspawn "mpc next")
    , ("<XF86AudioPrev>", logspawn "mpc prev")
    ] where
        logspawn cmd = spawn cmd >> runLogHook

