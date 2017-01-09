{-# LANGUAGE ForeignFunctionInterface #-}
module XMonad.Util.XResources 
    ( xresources
    , XResources (..)
    ) where

import Data.Maybe
import Graphics.X11.Xlib
import System.IO.Unsafe (unsafePerformIO)

import Foreign
import Foreign.C.String

getDefault :: Display -> String -> String -> IO (Maybe String)
getDefault dpy prog opt = withCString prog $ \c_prog ->
    withCString opt $ \c_opt -> do
        s <- xGetDefault dpy c_prog c_opt
        if s == nullPtr
           then return Nothing
           else Just <$> peekCString s
foreign import ccall unsafe "HsXlib.h XGetDefault"
    xGetDefault :: Display -> CString -> CString -> IO CString

xdefault :: String -> IO (Maybe String)
xdefault xd
  = openDisplay "" >>= (\dpy -> getDefault dpy "*" xd)

data XResources = XR
    { foreground :: String
    , background :: String
    , color0     :: String
    , color1     :: String
    , color2     :: String
    , color3     :: String
    , color4     :: String
    , color5     :: String
    , color6     :: String
    , color7     :: String
    , color8     :: String
    , color9     :: String
    , color10    :: String
    , color11    :: String
    , color12    :: String
    , color13    :: String
    , color14    :: String
    , color15    :: String
    , font       :: String
    } deriving Show

xresources' :: IO XResources
xresources' = do
    fg <- xdefault "foreground"
    bg <- xdefault "background"
    c0 <- xdefault "color0"
    c1 <- xdefault "color1"
    c2 <- xdefault "color2"
    c3 <- xdefault "color3"
    c4 <- xdefault "color4"
    c5 <- xdefault "color5"
    c6 <- xdefault "color6"
    c7 <- xdefault "color7"
    c8 <- xdefault "color8"
    c9 <- xdefault "color9"
    c10 <- xdefault "color10"
    c11 <- xdefault "color11"
    c12 <- xdefault "color12"
    c13 <- xdefault "color13"
    c14 <- xdefault "color14"
    c15 <- xdefault "color15"
    fn  <- xdefault "font"
    return XR
        { foreground = fromMaybe "#000000" fg
        , background = fromMaybe "#c0c0c0" bg
        , color0     = fromMaybe "#000000" c0
        , color1     = fromMaybe "#800000" c1
        , color2     = fromMaybe "#008000" c2
        , color3     = fromMaybe "#808000" c3
        , color4     = fromMaybe "#000080" c4
        , color5     = fromMaybe "#800080" c5
        , color6     = fromMaybe "#008080" c6
        , color7     = fromMaybe "#c0c0c0" c7
        , color8     = fromMaybe "#808080" c8
        , color9     = fromMaybe "#ff0000" c9
        , color10    = fromMaybe "#00ff00" c10
        , color11    = fromMaybe "#ffff00" c11
        , color12    = fromMaybe "#0000ff" c12
        , color13    = fromMaybe "#ff00ff" c13
        , color14    = fromMaybe "#00ffff" c14
        , color15    = fromMaybe "#ffffff" c15
        , font       = fromMaybe "-*-fixed-*-*-*-*-12-*-*-*-*-*-*-1" fn
        }

-- | this is 'safe' because I use fromMaybe.
xresources :: XResources
{-# NOINLINE xresources #-}
xresources = unsafePerformIO xresources'
