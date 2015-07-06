module Loader where
import qualified Data.Array as A
import Data.Function
import Colours

data Loaded = Loaded (A.Array (Int,Int) Codel)

type ID = Int

data Codel = Codel {
           _c :: Colour,
           _id :: ID
} deriving (Show, Eq)

eqColour :: Codel -> Codel -> Bool
eqColour = (==) `on` _c

eqID :: Codel -> Codel -> Bool
eqID = (==) `on` _id

loadImg :: FilePath -> IO Loaded
loadImg = undefined
