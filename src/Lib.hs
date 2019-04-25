{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Lib where

import           Data.Array.Accelerate                 as A
import           Data.Array.Accelerate.Data.Colour.RGB as A
import           Data.Array.Accelerate.Data.Complex    as A

import           Prelude                               as P

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

colorResult :: Acc (Array DIM2 Int32) -> Acc (Array DIM2 Colour)
colorResult = A.map color
  where
    color :: Exp Int32 -> Exp Colour
    color = undefined
    -- color (unlift -> x) = undefialtned

    colorPure :: Int32 -> Colour
    colorPure i = if i P.== maxBound then black else colorPick
      where
        black = RGB 1 1 1
        colorPick = case i `mod` 16 of
          0  -> RGB (66/255)  (30/255)  (15/255)
          1  -> RGB (25/255)  (7/255)   (26/255)
          2  -> RGB (9/255)   (1/255)   (47/255)
          3  -> RGB (4/255)   (4/255)   (73/255)
          4  -> RGB (0/255)   (7/255)   (100/255)
          5  -> RGB (12/255)  (44/255)  (138/255)
          6  -> RGB (24/255)  (82/255)  (177/255)
          7  -> RGB (57/255)  (125/255) (209/255)
          8  -> RGB (134/255) (181/255) (229/255)
          9  -> RGB (211/255) (236/255) (248/255)
          10 -> RGB (241/255) (233/255) (191/255)
          11 -> RGB (248/255) (201/255) (95/255)
          12 -> RGB (255/255) (170/255) (0/255)
          13 -> RGB (204/255) (128/255) (0/255)
          14 -> RGB (153/255) (87/255)  (0/255)
          15 -> RGB (106/255) (52/255)  (3/255)
