module XMonad.My.Preferences where

import XMonad.Hooks.Place

placement :: Placement
-- placement = withGaps (16,4,4,4) $ smart (0.5,0.5)
placement = simpleSmart
