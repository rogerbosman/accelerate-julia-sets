{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- mandelbrot
--     :: forall a. (Num a, RealFloat a, FromIntegral Int a, Elt (Complex a))
--     => Int                                  -- ^ image width
--     -> Int                                  -- ^ image height
--     -> Acc (Scalar a)                       -- ^ centre x
--     -> Acc (Scalar a)                       -- ^ centre y
--     -> Acc (Scalar a)                       -- ^ view width
--     -> Acc (Scalar Int32)                   -- ^ iteration limit
--     -> Acc (Scalar a)                       -- ^ divergence radius
--     -> Acc (Array DIM2 (Complex a, Int32))



height, width :: Int
height = 600
width = 1050

type IComplex = A.Complex Float


juliaSet :: Acc (Array DIM2 IComplex)
juliaSet = A.generate (A.constant (Z :. height :. width)) calcSingle
  where
    calcSingle :: Exp ((Z :. Int) :. Int) -> Exp IComplex
    calcSingle = undefined
