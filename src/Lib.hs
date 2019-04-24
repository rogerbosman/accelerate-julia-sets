{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Lib where

import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A

someFunc :: IO ()
someFunc = putStrLn "someFunc"

height, width :: Int
height = 600
width = 1050

type IComplex = A.Complex Float


juliaSet :: Acc (Array DIM2 IComplex)
juliaSet = A.generate (A.constant (Z :. height :. width)) calcSingle
  where
    calcSingle :: Exp DIM2 -> Exp IComplex
    calcSingle = undefined
    -- calcSingle (A.unlift -> Z :. (y :: Int) :. x) = undefined

test :: Exp DIM2 -> Exp Int32
test (A.unlift -> (Z :. (y :: Int) :. (x :: Int))) = undefined
