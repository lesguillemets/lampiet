module Colours where

data Lightness = Light | Normal | Dark
               deriving (Bounded, Enum, Show, Eq)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta
         deriving (Bounded, Enum, Show, Eq)

data Colour = White | Black | Chromatic { _l::Lightness, _h:: Hue}
            deriving Eq

instance Show Colour where
    show White = "White"
    show Black = "Black"
    show (Chromatic h l) = show h ++ show l

-- |
-- >>> print $ Chromatic Light Blue
-- LightBlue
