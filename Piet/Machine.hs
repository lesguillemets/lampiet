module Machine where

import Helpers

type Mem = [Integer]
data Machine = Machine {
             dirPointer :: Direction,
             codelChooser :: Direction,
             mem :: [Integer]
} deriving (Show)

