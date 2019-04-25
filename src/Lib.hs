{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Lib where

import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A

import           Prelude                            as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

height, width :: Int
height = 600
width = 1050

cvals :: (Float, Float)
cvals = (-0.7, 0.279)

maxIters :: Int32
maxIters = 255

type IComplex = A.Complex Float


startingSet :: Acc (Array DIM2 IComplex)
startingSet = A.generate (A.constant (Z :. height :. width)) calcSingle
  where
    calcSingle :: Exp DIM2 -> Exp IComplex
    calcSingle = undefined
    -- calcSingle (A.unlift -> Z :. (y :: Int) :. x) = undefined

    calcSinglePure :: (Int, Int) -> IComplex
    calcSinglePure (P.fromIntegral -> x, P.fromIntegral -> y) = zx :+ zy
      where
        zx = (-2.5) + (x/1050) * 3.5
        zy = (-1)   + (y/600)  * 2

-- test :: Exp DIM2 -> Exp Int32
-- test (A.unlift -> (Z :. (y :: Int) :. (x :: Int))) = undefined

iterateJulia :: Acc (Array DIM2 IComplex) -> Acc (Array DIM2 Int32)
iterateJulia = A.map iter
  where
    iter :: Exp IComplex -> Exp Int32
    iter = undefined
    -- iter (unlift -> x) = undefined

    iterPure :: IComplex -> Int32
    iterPure = iterIPure 0

    iterIPure :: Int32 -> IComplex -> Int32
    iterIPure i _          | i P.> maxIters = maxBound
    iterIPure i (zx :+ zy) | (zx * zx) + (zy + zy) P.< 4 =
      let (cx, cy) = cvals
          zx' = zx * zx - zy * zy + cx
          zy' = 2 * zx * zy  + cy
      in  iterIPure (i+1) (zx' :+ zy')
    iterIPure i _          = i

colorResult :: Acc (Array DIM2 Int32) -> Acc (Array DIM2 Int32)
iterateJulia = A.map color
  where
    color :: Exp Int32 -> Exp Int32
    color = undefined
    -- color (unlift -> x) = undefined

    colorPure :: Int32 -> Int32
    colorPure i = undefined
