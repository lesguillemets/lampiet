module Interpreter where

import Commands
import Direction
import Helpers
import Codel
import Machine

tryMove :: Loaded -> Direction -> RL -> Loc -> Maybe Loc
tryMove l d rl location =
        let nextp = moveStep d $ findNext l d rl location
            in
                if validLoc l nextp
                    then Just nextp
                    else Nothing

run :: Loaded -> Machine -> IO Machine
run l m = run' m 0 where
    run' m 8 = return m
    run' m counter =
        let
            next = tryMove l (dirPointer m) (codelChooser m) (loc m)
        in case next of
               Nothing -> run' (if counter `mod` 2 == 0
                                     then toggleCC m
                                     else turnDP m) (counter + 1)
               (Just nextLoc) -> let
                   c0 = l `colourAt` (loc m)
                   c1 = l `colourAt` nextLoc
                   area = l `areaAt` (loc m)
                   com = fromColours c0 c1
                   in applyMaybe com (fromIntegral area) (moveTo m nextLoc)
                        >>= run l
