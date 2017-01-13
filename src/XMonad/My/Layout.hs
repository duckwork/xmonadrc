{-# OPTIONS -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XMonad.My.Layout where

import XMonad hiding (layoutHook, (|||), tile)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts hiding (Toggle)
import XMonad.Layout.LayoutCombinators

layoutHook
  = avoidStruts
  . lessBorders OnlyFloat
  . mkToggle (single NBFULL)
  $ mrt ||| fullscreen
  where
    mrt 
      = gaps (zip [U .. L] (repeat $ fromIntegral gapwidth))
      $ toggleLayouts mirror tile
    tile 
      = mouseResizableTile
        { masterFrac = ratio
        , slaveFrac = ratio
        , fracIncrement = delta
        , draggerType = FixedDragger gapwidth (2*gapwidth)
        }
    mirror = tile { isMirrored = True }
    fullscreen = noBorders Full
    ------
    ratio = toRational ((sqrt 5 - 1) / 2) -- golden ratio
    delta = 1/25
    gapwidth = 4
