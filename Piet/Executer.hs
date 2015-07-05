{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Executer where

import Helpers

type Mem = [Integer]
data Machine = Machine {
             dirPointer :: Direction,
             codelChooser :: Direction,
             mem :: [Integer]
} deriving (Show)

data Command = Push
             | Pop
             | Add
             | Subtract
             | Multiply
             | Divide
             | Mod
             | Not
             | Greater
             | Pointer
             | Switch
             | Duplicate
             | Roll
             | In
             | Out
             deriving (Show, Read)

apply :: Command -> Integer -> Machine -> IO Machine

apply Push n m@(Machine {..}) = return $ m{ mem = n:mem }
apply Pop _  m@(Machine {..}) = return $ m{ mem = tail mem}
apply Add _ m@(Machine{mem = (x:y:xs), ..}) = return $ m {mem = (x+y):xs}
-- apply Subtract _ (x:y:xs) = return $ (y-x):xs
-- apply Multiply _ (x:y:xs) =  return $ (x*y):xs
-- -- simply ignoreingdevides by zero
-- apply Divide _ r@(x:y:xs) = case x of
--                                 0 -> return r
--                                 _ -> return $ (y `div` x):xs
-- apply Mod _ r@(x:y:xs) = case x of
--                              0 -> return r
--                              _ -> return $ (y `mod` x):xs
