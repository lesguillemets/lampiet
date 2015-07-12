{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
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

turnDP :: Machine -> Machine
turnDP m@(Machine {..}) = m {dirPointer = turnRight dirPointer}

toggleCC :: Machine -> Machine
toggleCC m@(Machine {..}) = m {codelChooser = flipRL codelChooser}

moveTo :: Machine -> Loc -> Machine
moveTo m@(Machine {..}) newloc = m {loc = newloc}
