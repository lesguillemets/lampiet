module Interpreter where

import Commands
import Direction
import Helpers
import Loader
import Machine

tryMove :: Loaded -> Direction -> RL -> Loc -> Maybe Loc
tryMove l d rl loc =
        let nextp = moveStep d $ findNext l d rl loc
            in
                if validLoc nextp
                    then Just nextp
                    else Nothing


