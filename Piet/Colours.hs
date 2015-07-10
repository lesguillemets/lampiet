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

cv0 :: Int
cv1 :: Int
cv2 :: Int
cv0 = 0
cv1 = 192 -- c0
cv2 = 255 -- ff

defaultColour :: Colour
defaultColour = White

fromRGB :: Integral a => (a,a,a) -> Colour
fromRGB (r', g', b') =
    let
        r = fromIntegral r'
        g = fromIntegral g'
        b = fromIntegral b'
        s = minimum [r,g,b]
        l = maximum [r,g,b]
        lightness
            | s == cv0 && l == cv2 = Just Normal
            | s == cv1 && l == cv2 = Just Light
            | s == cv0 && l == cv1 = Just Dark
            | otherwise = Nothing
        h = case (r `compare` g, g `compare` b, b `compare` r) of
                (GT,EQ,LT) -> Just Red
                (EQ,GT,LT) -> Just Yellow
                (LT,GT,EQ) -> Just Green
                (LT,EQ,GT) -> Just Cyan
                (EQ,LT,GT) -> Just Blue
                (GT,LT,EQ) -> Just Magenta
                _ -> Nothing
        in
            if l == cv0
                then Black
                else
                    case (h,lightness) of
                        (Just hu, Just li) -> Chromatic li hu
                        _  -> defaultColour
-- |
-- >>> fromRGB (255,255,255)
-- White
-- >>> fromRGB (192,192,255)
-- LightBlue
-- >>> fromRGB (192,255,156)
-- White
-- >>> fromRGB (0,192,192)
-- DarkCyan

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
