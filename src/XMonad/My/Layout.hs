module XMonad.My.Layout where

import XMonad hiding (layoutHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ToggleLayouts hiding (Toggle)
import XMonad.Layout.SimplestFloat
---
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.PositionStoreFloat

layoutHook
  = avoidStruts
  . lessBorders OnlyFloat
  . mkToggle (single NBFULL)
  $ mrt ||| float
  where
    mrt 
      = gaps (zip [U .. L] (repeat $ fromIntegral gapwidth))
      $ toggleLayouts (rn "[TT]" mirror) (rn "[|=]" tile)
    tile 
      = mouseResizableTile
        { masterFrac = ratio
        , slaveFrac = ratio
        , fracIncrement = delta
        , draggerType = FixedDragger gapwidth (2*gapwidth)
        }
    mirror = tile { isMirrored = True }
    -- float = floatingDeco $ borderResize $ positionStoreFloat
    -- floatingDeco = noFrillsDeco shrinkText def
    float = simplestFloat
    rn s = renamed [Replace s]
    ------
    ratio = toRational ((sqrt 5 - 1) / 2) -- golden ratio
    delta = 1/25
    gapwidth = 4
