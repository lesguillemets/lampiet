module Helpers (
               cSucc, cPred, cycularMove,
               Direction (..),
               RL (..),
               roll
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

data Direction = Up | Rt | Dw | Lf deriving (Bounded,Enum,Show)

data RL = RLRight | RLLeft deriving (Bounded,Enum,Show)

roll :: Int -> Int -> [a] -> [a]
roll depth n xs = let
    (pre,post) = splitAt depth xs
    in
        rotate n  pre ++ post

rotate :: Int -> [a] -> [a]
rotate n xs = let
    l = length xs
    n' = if n > 0 then n
                  else l + n
        in
            take l . drop n' . cycle $ xs
-- |
-- >>> rotate 2 [1..4]
-- [3,4,1,2]
-- >>> rotate (-1) [1..4]
-- [4,1,2,3]
