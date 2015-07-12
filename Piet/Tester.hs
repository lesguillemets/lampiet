module Tester where
import Colours
import Commands
import Direction
import Machine
import Codel
import Interpreter
import Data.List (foldl')
import Data.Maybe
import Data.Array.IArray (listArray)
import Control.Monad

-- sample : Hi
hiLoaded :: Loaded
hiLoaded = Loaded $ listArray ((0,0),(9,9))
    [
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Blue) 0 9,
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Red) 1 12, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Yellow) 2 4, Codel (Chromatic Normal Yellow) 2 4,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 4 2,
    --
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Blue) 0 9,
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Red) 1 12, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Yellow) 2 4, Codel (Chromatic Normal Yellow) 2 4,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 4 2,
    --
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Blue) 0 9,
    Codel (Chromatic Normal Blue) 0 9, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Red) 1 12, Codel (Chromatic Normal Red) 1 12,
    Codel Black 3 16, Codel Black 3 16,
    Codel Black 3 16, Codel Black 3 16,
    --
    Codel (Chromatic Dark Blue) 5 8, Codel (Chromatic Dark Blue) 5 8,
    Codel (Chromatic Dark Blue) 5 8, Codel (Chromatic Normal Red) 1 12,
    Codel (Chromatic Normal Red) 1 12, Codel (Chromatic Normal Red) 1 12,
    Codel Black 3 16, Codel (Chromatic Normal Green) 6 6,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 7 4,
    --
    Codel (Chromatic Dark Blue) 5 8, Codel (Chromatic Dark Blue) 5 8,
    Codel (Chromatic Dark Blue) 5 8, Codel Black 8 1,
    Codel (Chromatic Normal Green) 6 6, Codel (Chromatic Normal Green) 6 6,
    Codel (Chromatic Normal Green) 6 6, Codel (Chromatic Normal Green) 6 6,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 7 4,
    --
    Codel (Chromatic Dark Blue) 5 8, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Dark Blue) 5 8, Codel (Chromatic Normal Red) 9 2,
    Codel (Chromatic Dark Cyan) 10 1, Codel (Chromatic Normal Red) 11 2,
    Codel Black 3 16, Codel (Chromatic Normal Green) 6 6,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 7 4,
    --
    Codel Black 12 1, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Light Blue) 13 1, Codel (Chromatic Normal Red) 9 2,
    Codel (Chromatic Normal Cyan) 14 10, Codel (Chromatic Normal Red) 11 2,
    Codel Black 3 16, Codel Black 3 16,
    Codel Black 3 16, Codel (Chromatic Normal Yellow) 7 4,
    --
    Codel (Chromatic Normal Green) 8 11, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Dark Magenta) 15 1, Codel (Chromatic Normal Cyan) 14 10,
    Codel (Chromatic Normal Cyan) 14 10, Codel (Chromatic Normal Cyan) 14 10,
    Codel (Chromatic Normal Cyan) 14 10, Codel (Chromatic Normal Cyan) 14 10,
    Codel Black 3 16, Codel Black 3 16,
    --
    Codel (Chromatic Normal Green) 8 11, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Dark Cyan) 16 1, Codel (Chromatic Normal Red) 17 1,
    Codel (Chromatic Normal Cyan) 14 10, Codel (Chromatic Dark Green) 18 3,
    Codel (Chromatic Dark Green) 18 3, Codel (Chromatic Normal Cyan) 14 10,
    Codel (Chromatic Normal Cyan) 14 10, Codel (Chromatic Normal Cyan) 14 10,
    --
    Codel Black 19 1, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Normal Green) 8 11, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Normal Green) 8 11, Codel (Chromatic Normal Green) 8 11,
    Codel (Chromatic Dark Green) 18 3, Codel (Chromatic Light Green) 19 1,
    Codel (Chromatic Dark Cyan) 20 1, Codel (Chromatic Dark Blue) 21 1
    ]

-- |
-- >>> _ <- run hiLoaded initialMachine
-- Hi

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
