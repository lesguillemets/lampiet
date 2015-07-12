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
towards :: Direction -> Loc -> Loc -> Ordering

flipRL :: RL -> RL
flipRL RLRight = RLLeft
flipRL RLLeft = RLRight

towards Up (_,y0) (_,y) = y0 `compare` y
towards Dw (_,y0) (_,y) = y `compare` y0
towards Rt (x0,_) (x,_) = x `compare` x0
towards Lf (x0,_) (x,_) = x0 `compare` x
-- |
-- towards direction loc0 loc1 :
-- is <loc1> is on <direction> when looked from <loc0>?
-- >>> towards Up (5,5) (10,2)
-- GT
-- >>> towards Rt (2,3) (5,0)
-- GT

look :: Direction -> RL -> Direction
look d RLRight = circularMove d 1
look d RLLeft = circularMove d (-1)
-- |
-- >>> look Rt RLRight
-- Dw
-- >>> look Dw RLLeft
-- Rt

moveStep :: Direction -> Loc -> Loc
moveStep Up (x,y) = (x,y-1)
moveStep Dw (x,y) = (x,y+1)
moveStep Rt (x,y) = (x+1,y)
moveStep Lf (x,y) = (x-1,y)
