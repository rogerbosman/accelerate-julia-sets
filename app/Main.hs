module Main where

import           Graphics.Gloss
import           Lib

import           Data.Array.Accelerate                  (Array, DIM1, DIM2, Elt,
                                                         Scalar, Word32)
import qualified Data.Array.Accelerate                  as A hiding (round)
import           Data.Array.Accelerate.Data.Colour.RGB  as A
import           Data.Array.Accelerate.LLVM.PTX         as A
import           Graphics.Gloss.Accelerate.Data.Picture as A

import           Prelude                               as P hiding ((<), (>), (&&), fst, snd, (==))


main :: IO ()
main = animate
  (InWindow "Julia set simulation" (2100, 1200) (0, 0))
  black
  makePicture

makePicture :: Float -> Picture
makePicture i = A.bitmapOfArray juliaArray False

juliaArray :: Array DIM2 Word32
juliaArray = run runJulia
