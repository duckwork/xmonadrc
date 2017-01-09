{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ToggleFade
-- Copyright    : (c) Case Duckworth 2017
-- License      : WTFPL
--
-- Maintainer   : cased123@gmail.com
-- Stability    : unstable
-- Portability  : not portable
--
-- Module to toggle transparency effects of windows in xmonad.
--
-----------------------------------------------------------

module XMonad.Hooks.ToggleFade 
    ( -- * Usage
      -- $usage
      NoFadeSet (..)
    , doFade
    , toggleFadeLogHook
    , toggleFade
    , toggleFadeLogHook'
    -- , toggleFade'
    ) where

-- $usage
-- This module allows the toggling of window fading on-the-fly,
-- using 'XMonad.Hooks.FadeInactive' and 
-- 'XMonad.Util.ExtensibleState.'  To use it, add @toggleFadeHook@
-- to your logHook and add a keybind that uses @toggleFade@.  If
-- you want to add other window 'Query's besides just 'isUnfocused',
-- you can use 'toggleFadeLogHook\''.

import Control.Monad (join)
import XMonad
import XMonad.Hooks.FadeInactive
import qualified Data.Set as S
import qualified XMonad.Util.ExtensibleState as XS

-- | datatype storing the set of windows we don't want to fade
newtype NoFadeSet = NoFadeSet
    { getNFS :: S.Set Window
    } deriving (Read, Show, Typeable)

instance ExtensionClass NoFadeSet where
    initialValue = NoFadeSet S.empty
    extensionType = PersistentExtension

-- | You want to fade windows that *aren't* in the set.
doFade :: Window -> X Bool
doFade w = XS.gets (S.notMember w . getNFS)

-- | Given a query of windows you want to stay opaque and
-- the amount to fade other windows by, set up a log hook.
toggleFadeLogHook' :: Query Bool -> Rational -> X ()
toggleFadeLogHook' shouldFade opacity
  = fadeOutLogHook $ fadeIf shouldFade' opacity
  where
    shouldFade' = shouldFade 
                <&&> (join . asks $ \w -> liftX $ doFade w)

-- | A basic toggleFadeLogHook' where all unfocused windows are faded.
toggleFadeLogHook :: Rational -> X ()
toggleFadeLogHook = toggleFadeLogHook' isUnfocused

-- | The pure function that will toggle fading for a window.
toggleFade' :: Window -> NoFadeSet -> NoFadeSet
toggleFade' w s
  | S.member w s' = NoFadeSet $ S.delete w s'
  | otherwise     = NoFadeSet $ S.insert w s'
  where s' = getNFS s

-- | A keybind-ready function that will toggle a window's fading.
toggleFade :: Window -> X ()
toggleFade = XS.modify . toggleFade'
