{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Commands where

import Control.Applicative
import Data.Char

import Machine
import Colours
import Direction
import Helpers

updMemWith :: ([Integer] -> [Integer]) -> Machine -> Machine
updMemWith f m@(Machine {..}) = m {mem = f mem}
updMemWithOp :: (Integer -> Integer -> Integer) -> Machine -> Machine
updMemWithOp _ m@(Machine {mem=[]}) = m
updMemWithOp _ m@(Machine {mem=[_]}) = m
updMemWithOp op m = flip updMemWith m $ \ (x:y:xs) -> (y `op` x):xs

data Command = Skip
             | Push
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
             | InInt
             | InChar
             | OutInt
             | OutChar
             deriving (Show, Read, Enum)

-- TODO : handle white
fromColours :: Colour -> Colour -> Maybe Command
fromColours x y = (\(l,h) -> toEnum $ 3*h + l) <$> colourDiff x y
-- |
-- >>> fromColours (Chromatic Normal Blue) (Chromatic Light Yellow)
-- Just Switch

apply :: Command -> Integer -> Machine -> IO Machine
-- the second argument is the value of the colour.

apply Skip _ m = return m

apply Push n m = return . updMemWith (n:) $ m
apply Pop _ m@(Machine{mem=[], ..}) = return m
apply Pop _  m = return . updMemWith tail $ m

apply Add _ m = return . updMemWithOp (+) $ m
apply Subtract _ m = return . updMemWithOp (-) $ m
apply Multiply _ m = return . updMemWithOp (*) $ m
-- TODO: simply ignoreingdevides by zero
apply Divide _ m = return . updMemWithOp div $ m
apply Mod _ m = return . updMemWithOp mod $ m

apply Not _ m@(Machine{mem=[], ..}) = return m
apply Not _ m = return . updMemWith
    (\(x:r) -> if x == 0 then 1:r else 0:r) $ m
apply Greater _ m = return . updMemWithOp
    (((toInteger . fromEnum) . ) .  (>)) $ m

apply Pointer _ m@(Machine{mem=[], ..}) = return m
apply Pointer _ m@(Machine{mem=(h:r), ..}) =
        return $ m {
                   dirPointer = circularMove dirPointer (fromInteger h),
                   mem=r
                   }

apply Switch _ m@(Machine{mem=[], ..}) = return m
apply Switch _ m@(Machine{mem=(h:r), ..}) =
        return $ m {
                   codelChooser = circularMove codelChooser (fromInteger h),
                   mem = r
        }

apply Duplicate _ m@(Machine{mem=[], ..}) = return m
apply Duplicate _ m = return . updMemWith (\(x:r) -> x:x:r) $ m

apply Roll _ m@(Machine{mem=[], ..}) = return m
apply Roll _ m@(Machine{mem=[_], ..}) = return m
apply Roll _ m@(Machine {mem=(x:y:r), ..}) =
        return $ m {mem = roll (fromInteger y) (fromInteger x) r}

apply InInt _ m = do
    n <- readLn
    return . updMemWith (n:) $ m

apply InChar _ m = do
    n <- toInteger . ord . head <$> getLine
    return . updMemWith (n:) $ m

apply OutInt _ m@(Machine{mem=[], ..}) = return m
apply OutInt _ m@(Machine{mem=(h:r), ..}) = do
    print h
    return $ m{ mem = r}

apply OutChar _ m@(Machine{mem=[], ..}) = return m
apply OutChar _ m@(Machine{mem=(h:r), ..}) = do
    putChar . chr . fromInteger $ h
    return $ m{ mem = r}

applyMaybe :: Maybe Command -> Integer -> Machine -> IO Machine
applyMaybe Nothing _ m = return m
applyMaybe (Just c) i m = apply c i m
