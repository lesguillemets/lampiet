module Helpers (
               circularMove,
               circDiff,
               roll
               ) where

-- $setup
-- >>> data Foo = A | B | C | D | E deriving (Bounded, Enum, Show)

cSucc :: (Bounded a, Enum a) => a -> a
cSucc = flip circularMove 1
-- |
-- >>> cSucc 'a'
-- 'b'
-- >>> cSucc True
-- False
-- >>> cSucc . cSucc $ D
-- A

cPred :: (Bounded a, Enum a) => a -> a
cPred = flip circularMove (-1)
-- |
-- >>> cPred 'b'
-- 'a'
-- >>> cPred False
-- True

circularMove :: (Bounded a, Enum a) => a -> Int -> a
circularMove e n = let
    m = 1+fromEnum (maxBound `asTypeOf` e)
    in
        toEnum . (`rem` m) $ n + fromEnum e + m

-- TODO : perhaps we'd better flip
circDiff :: (Bounded a, Enum a) => a -> a -> Int
circDiff x y = let
    m = 1 + fromEnum (maxBound `asTypeOf` x)
    d = (fromEnum y - fromEnum x) `rem` m
    in
       if d < 0 then d+m else d
-- |
-- >>> circDiff B E -- B -> C -> D -> E
-- 3
-- >>> circDiff D B -- D -> E -> A -> b
-- 3

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
