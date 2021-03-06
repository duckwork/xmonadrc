module XMonad.My.Helpers where

import Control.Monad
import Data.List
import Data.Maybe
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.WithAll
import XMonad.Hooks.ManageDocks
-- import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.Util.PositionStore
import XMonad.Util.SpawnNamedPipe (spawnNamedPipe)
import qualified Data.Map as M
import qualified XMonad.My.Theme as My
import qualified XMonad.StackSet as W

runLogHook :: X ()
runLogHook = join $ asks $ logHook . config

toggleFloat :: Window -> X ()
toggleFloat w = do
    floats <- gets (W.floating . windowset)
    if w `M.member` floats
       then withFocused $ windows . W.sink
       else psPlace w

toggleFloatAll :: X ()
toggleFloatAll = do
    floats <- gets (W.floating . windowset)
    if M.null floats
       then withAll psPlace
       else sinkAll

psPlace :: Window -> X ()
psPlace win = withDisplay $ \d -> do
    ps <- getPosStore
    ws <- gets windowset
    wa <- io $ getWindowAttributes d win
    sc <- fromMaybe (W.current ws) <$> pointScreen (fromIntegral $ wa_x wa) (fromIntegral $ wa_y wa)
    let sr = screenRect . W.screenDetail $ sc
        mwr = posStoreQuery ps win sr
    case mwr of
      Just (Rectangle x y w h) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            w' = fromIntegral w - fromIntegral (wa_width wa)
            h' = fromIntegral h - fromIntegral (wa_height wa)
        keysMoveWindowTo (x',y') (0,0) win
        keysResizeWindow (w',h') (0,0) win
        -- focus win >> placeFocused pl
      Nothing -> return ()

setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    fs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ do
        supportedList <- (join . maybeToList) <$> getWindowProperty32 dpy a r
        changeProperty32 dpy r a aTOM propModeReplace (nub $ fromIntegral fs : supportedList)

lemonbar :: X ()
lemonbar = spawnNamedPipe lemonbar' "xpanel"
  where
    lemonbar' 
      = unwords
        [ "lemonbar" -- , "-o -2"
        , "-n xpanel"
        , "-u 2", "-U '" ++ My.normalFG ++ "'"
        , "-f '" ++ My.lemonFont ++ "'"
        , "-B '#00" ++ tail My.normalBG ++ "'"
        , "-F '" ++ My.normalFG ++ "'"
        , "| sh"
        ]

strutsOn :: X ()
strutsOn = sendMessage $ SetStruts [minBound .. maxBound] []

strutsOff :: X ()
strutsOff = sendMessage $ SetStruts [] [minBound .. maxBound]

fullScreenFocused :: X ()
fullScreenFocused = withFocused 
                  $ \w -> windows $ W.float w (W.RationalRect 0 0 1 1)
