module Loader where
import qualified Data.Array as A
import Data.Function
import Colours
import Direction
import Helpers

newtype Loaded = Loaded (A.Array Loc Codel)

data Codel = Codel {
           _c :: Colour,
           _id :: ID,
           _area :: Int
} deriving (Show, Eq)

eqColour :: Codel -> Codel -> Bool
eqColour = (==) `on` _c

eqID :: Codel -> Codel -> Bool
eqID = (==) `on` _id

loadImg :: FilePath -> IO Loaded
loadImg = undefined
