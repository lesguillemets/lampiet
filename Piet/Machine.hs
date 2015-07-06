module Machine where

import Helpers

type Mem = [Integer]

data Machine = Machine {
             dirPointer :: Direction,
             codelChooser :: RL,
             mem :: [Integer]
} deriving (Show)

initialMachine :: Machine
initialMachine = Machine Rt RLLeft []
