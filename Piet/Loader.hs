module Loader where
import qualified Data.Array as A
import Data.Array ((!))
import Data.Function
import Data.List

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

connectedArea :: Loaded -> Loc -> [Loc]
connectedArea (Loaded l) loc =
    map fst . filter (eqID (l!loc) . snd) $ A.assocs l

findEdge :: Loaded -> Direction -> Loc -> [Loc]
findEdge l d loc =
        head . groupBy (\l0 l1 -> towards d l0 l1 == EQ) -- take farthests
            . sortBy (flip (towards d)) -- sort by how far
            $ connectedArea l loc -- Codels connected to this
