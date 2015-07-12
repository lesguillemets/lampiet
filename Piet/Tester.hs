module Tester where
import Colours
import Commands
import Direction
import Machine
import Loader
import Data.List (foldl')
import Data.Maybe
import Data.Array.IArray (listArray)
import Control.Monad

-- the trace of the sample program
-- (
--
trace0 :: [(Colour,Integer)]
trace0 = zip [
             Chromatic Normal Red, Chromatic Dark Red,
             Chromatic Normal Magenta, Chromatic Dark Magenta,
             Chromatic Normal Blue, Chromatic Dark Blue,
             Chromatic Dark Green, Chromatic Dark Red,
             Chromatic Normal Magenta, Chromatic Light Blue,
             Chromatic Normal Blue, Chromatic Normal Green,
             Chromatic Light Yellow, Chromatic Normal Yellow,
             Chromatic Light Red, Chromatic Normal Red,
             Chromatic Light Magenta, Chromatic Dark Blue,
             Chromatic Light Blue, Chromatic Dark Cyan,
             Chromatic Normal Green, Chromatic Dark Green,
             Chromatic Normal Yellow, Chromatic Dark Yellow,
             Chromatic Normal Red ]
         [
         72,1,101,3,108,2,2,1,1,111,5,5,32,16,119,10,3,114,1,1,100,1,33,1,4
         ]
-- |
-- >>> followTrace trace0
-- Hello world!

followTrace :: [(Colour,Integer)] -> IO ()
followTrace t = do
    let m = initialMachine
        cmds = zipWith
                (\(c0,x) (c1,_) -> (fromColours c0 c1,x)) t (tail t)
    ml <- flip connect m $ map (\(c,w) -> apply (fromJust c) w) cmds
    return ()

sampleLoaded :: Loaded
-- LB DG NM
-- LB LB NM
-- LB NM NM
sampleLoaded = Loaded $ listArray ((0,0),(2,2))
        [
        Codel (Chromatic Light Blue) 0 4,
        Codel (Chromatic Light Blue) 0 4,
        Codel (Chromatic Light Blue) 0 4,
        Codel (Chromatic Dark Green) 1 1,
        Codel (Chromatic Light Blue) 0 4,
        Codel (Chromatic Normal Magenta) 2 4,
        Codel (Chromatic Normal Magenta) 2 4,
        Codel (Chromatic Normal Magenta) 2 4,
        Codel (Chromatic Normal Magenta) 2 4
        ]
-- |
-- >>> findEdge sampleLoaded Rt (0,0)
-- [(1,1)]
-- >>> findEdge sampleLoaded Lf (0,0)
-- [(0,0),(0,1),(0,2)]
-- >>> findEdge sampleLoaded Dw (0,0)
-- [(0,2)]
-- >>> findEdge sampleLoaded Up (0,0)
-- [(0,0)]
-- >>> findEdge sampleLoaded Dw (1,0)
-- [(1,0)]
-- >>> findEdge sampleLoaded Lf (2,0)
-- [(1,2)]

-- |
-- >>> findNext sampleLoaded Dw RLRight (2,0)
-- (1,2)

connect :: Monad m => [a -> m a] -> a -> m a
connect = foldl' (>=>) return
