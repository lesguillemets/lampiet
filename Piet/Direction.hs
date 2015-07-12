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

-- FIXME :: saner inprementation
towards :: Direction -> Loc -> Loc -> Bool
towards Up (_,y0) (_,y) = y <= y0
towards Dw (_,y0) (_,y) = y0 <= y
towards Rt (x0,_) (x,_) = x0 <= x
towards Lf (x0,_) (x,_) = x <= x0
-- |
-- towards direction loc0 loc1 :
-- is <loc1> is on <direction> when looked from <loc0>?
-- >>> towards Up (5,5) (10,2)
-- True
-- >>> towards Rt (2,3) (5,0)
-- True
