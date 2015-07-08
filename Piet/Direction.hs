module Direction where

import Helpers

data Direction = Up | Rt | Dw | Lf deriving (Bounded,Enum,Show)

data RL = RLRight | RLLeft deriving (Bounded,Enum,Show)


flipDir :: Direction -> Direction
flipDir = flip circularMove 2
-- |
-- >>> flipDir Up
-- Dw
-- >>> flipDir Lf
-- Rt

turnRight :: Direction -> Direction
turnRight = flip circularMove 1
-- |
-- >>> map turnRight $ enumFrom Up
-- [Rt,Dw,Lf,Up]
