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

updMemWith :: ([Integer] -> [Integer]) -> Machine -> Machine
updMemWith f m@(Machine {..}) = m {mem = f mem}
updMemWithOp :: (Integer -> Integer -> Integer) -> Machine -> Machine
updMemWithOp op = updMemWith $ \ (x:y:xs) -> (y `op` x):xs

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
-- the second argument is the value of the colour.

apply Push n m = return . updMemWith (n:) $ m
apply Pop _  m = return . updMemWith tail $ m

apply Add _ m = return . updMemWithOp (+) $ m
apply Subtract _ m = return . updMemWithOp (-) $ m
apply Multiply _ m = return . updMemWithOp (*) $ m
-- TODO: simply ignoreingdevides by zero
apply Divide _ m = return . updMemWithOp div $ m
apply Mod _ m = return . updMemWithOp mod $ m

apply Not _ m = return . updMemWith
    (\(x:r) -> if x == 0 then 1:r else 0:r) $ m
apply Greater _ m = return . updMemWithOp
    (((toInteger . fromEnum) . ) .  (>)) $ m

apply Pointer _ m@(Machine{mem=(h:r), ..}) =
        return $ m {
                   dirPointer = cycularMove dirPointer (fromInteger h),
                   mem=r
                   }

apply Switch _ m@(Machine{mem=(h:r), ..}) =
        return $ m {
                   codelChooser = cycularMove codelChooser (fromInteger h),
                   mem = r
        }

apply Duplicate _ m = return . updMemWith (\(x:r) -> x:x:r) $ m

-- TODO
apply Roll _ _ = undefined

apply In _ m = do
    n <- readLn
    return . updMemWith (n:) $ m

apply Out _ m@(Machine{mem=(h:r), ..}) = do
    print h
    return $ m{ mem = r}
