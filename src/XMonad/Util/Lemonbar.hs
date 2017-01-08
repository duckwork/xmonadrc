module XMonad.Util.Lemonbar 
    ( lbReverse, lbReverseL
    , lbAlignLeft, lbAlignCenter, lbAlignRight
    , lbColor, lbColorL
    , lbFontIdx, lbFontIdxL
    , lbOverline, lbOverlineL
    , lbUnderline, lbUnderlineL
    , lbLineColor, lbLineColorL
    , lbClick, lbClickL
    )where

-- convenience functions for lemonbar

import XMonad.Hooks.DynamicLog (wrap)
import XMonad.Util.Loggers (onLogger, Logger(..))

lbReverse :: String -> String
lbReverse = wrap "%{R}" "%{R}"

lbAlignLeft, lbAlignCenter, lbAlignRight :: String
lbAlignLeft = "%{l}"
lbAlignCenter = "%{c}"
lbAlignRight = "%{r}"

lbColor :: String -> String -> String -> String
lbColor fg bg = wrap before after where
    before
      = concat
      $ [ if null fg then "" else concat ["%{F", fg, "}"]
        , if null bg then "" else concat ["%{B", bg, "}"]
        ]
    after
      = concat
      $ [ if null fg then "" else "%{F-}"
        , if null bg then "" else "%{B-}"
        ]

lbFontIdx :: Int -> String -> String
lbFontIdx n s = concat [ "%{T", show n, "}", s, "%{T-}" ]

lbOverline :: String -> String
lbOverline = wrap "%{+o}" "%{-o}"

lbUnderline :: String -> String
lbUnderline = wrap "%{+u}" "%{-u}"

lbLineColor :: String -> String -> String
lbLineColor c s = concat [ "%{U", c, "}", s, "%{U-}" ]

lbClick :: Int -> String -> String -> String
lbClick button action 
  = wrap ("%{A"++(show button)++":"++action++":}") "%{A}"

-- logger functions

lbReverseL :: Logger -> Logger
lbReverseL = onLogger lbReverse

lbColorL :: String -> String -> Logger -> Logger
lbColorL fg bg = onLogger $ lbColor fg bg

lbFontIdxL :: Int -> Logger -> Logger
lbFontIdxL fn = onLogger $ lbFontIdx fn

lbOverlineL :: Logger -> Logger
lbOverlineL = onLogger lbOverline

lbUnderlineL :: Logger -> Logger
lbUnderlineL = onLogger lbUnderline

lbLineColorL :: String -> Logger -> Logger
lbLineColorL c = onLogger $ lbLineColor c

lbClickL :: Int -> String -> Logger -> Logger
lbClickL b a = onLogger $ lbClick b a
