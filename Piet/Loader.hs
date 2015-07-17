module Loader where

import Codec.Picture -- Juicypixel
import qualified Data.Array.IArray as IA
import qualified Data.Vector as V
import Data.Word (Word8)

import Codel
import Colours
import Helpers

loadImg :: FilePath -> IO Loaded
loadImg f = do
    (Right pict) <- readImg f
    return $ parse pict

readImg :: FilePath -> IO (Either String (IA.Array Loc (Word8,Word8,Word8)))
readImg fname = do
    img <- readImage fname
    case img of
        (Left s) -> return $ Left s
        (Right im) ->
            case im of
                (ImageRGB8 i) -> return $ Right (toArr i)
                _ -> return $ Left "unsupported color space?"


toArr :: Image PixelRGB8 -> IA.Array Loc (Word8,Word8,Word8)
toArr = undefined

parse :: IA.Array Loc (Word8,Word8,Word8) -> Loaded
parse = undefined

