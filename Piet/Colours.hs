module Colours where
import Helpers (circDiff)
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

colourDiff :: Colour -> Colour -> Maybe (Int, Int)
colourDiff (Chromatic h0 l0) (Chromatic h1 l1) =
        Just (circDiff h0 h1, circDiff l0 l1)
colourDiff _ _ = Nothing
-- |
-- >>> colourDiff (Chromatic Normal Red) (Chromatic Dark Blue)
-- Just (1,4)
-- >>> colourDiff Black (Chromatic Normal Red)
-- Nothing
-- >>> colourDiff (Chromatic Dark Blue) (Chromatic Normal Red)
-- Just (2,2)
