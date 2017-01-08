module XMonad.My.Workspaces where

import XMonad hiding (workspaces)

workspaces :: [WorkspaceId]
workspaces = map show ([1..9] :: [Integer])
