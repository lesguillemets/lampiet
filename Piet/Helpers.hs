module Helpers (
               cSucc, cPred,
               Direction (..),
               RL (..)
               ) where

cSucc :: (Bounded a, Enum a) => a -> a
cSucc = flip cycularMove 1
-- |
-- >>> cSucc 'a'
-- 'b'
-- >>> cSucc True
-- False

cPred :: (Bounded a, Enum a) => a -> a
cPred = flip cycularMove (-1)
-- |
-- >>> cPred 'b'
-- 'a'
-- >>> cPred False
-- True

cycularMove :: (Bounded a, Enum a) => a -> Int -> a
cycularMove e n = let
    m = 1+fromEnum (maxBound `asTypeOf` e)
    in
        toEnum . (`mod` m) $ n + fromEnum e + m

data Direction = Up | Rt | Dw | Lf deriving (Show)

data RL = RLRight | RLLeft deriving (Show)
