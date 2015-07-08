module Machine where

import Direction
import Helpers

type Mem = [Integer]

data Machine = Machine {
             dirPointer :: Direction,
             codelChooser :: RL,
             loc :: (Int,Int),
             mem :: [Integer]
} deriving (Show)

initialMachine :: Machine
initialMachine = Machine Rt RLLeft (0,0) []
