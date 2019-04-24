module Main where

import           Graphics.Gloss
import           Lib

import           Data.Array.Accelerate          (Array, DIM1, Elt, Scalar)
import qualified Data.Array.Accelerate          as A hiding (round)
import           Data.Array.Accelerate.LLVM.PTX as A

main :: IO ()
main = someFunc
