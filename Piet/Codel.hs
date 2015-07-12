module Codel where
import qualified Data.Array.IArray as IA
import Data.Array.IArray ((!))
import Data.Function
import Data.List

import Colours
import Direction
import Helpers

newtype Loaded = Loaded (IA.Array Loc Codel) deriving (Show)

colourAt :: Loaded -> Loc -> Colour
colourAt (Loaded l) loc = _c $ l ! loc

areaAt :: Loaded -> Loc -> Int
areaAt (Loaded l) loc = _area $ l ! loc

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
    map fst . filter (eqID (l!loc) . snd) $ IA.assocs l

findEdge :: Loaded -> Direction -> Loc -> [Loc]
findEdge l d loc =
        head . groupBy (\l0 l1 -> towards d l0 l1 == EQ) -- take farthests
            . sortBy (towards d) -- sort by how far
            $ connectedArea l loc -- Codels connected to this

findNext :: Loaded -> Direction -> RL -> Loc -> Loc
findNext l d rl loc =
    minimumBy (towards (look d rl)) $ findEdge l d loc

validLoc :: Loaded -> Loc -> Bool
validLoc (Loaded l) loc =
        IA.inRange (IA.bounds l) loc && _c (l!loc)  /= Black
