{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib where

import           Data.Array.Accelerate                 as A
import           Data.Array.Accelerate.Data.Colour.RGB as A
import           Data.Array.Accelerate.Data.Complex    as A

import           Prelude                               as P hiding (fst, snd,
                                                             (&&), (<), (==),
                                                             (>))

import           World

type IComplex = A.Complex Float
type IterFun = (Exp Float -> Exp Float -> (Exp Float, Exp Float) -> Exp IComplex)
type Zoom = Exp Float

cvals :: (Exp Float, Exp Float)
cvals = (-0.7, 0.279)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

height, width :: Int
height = 1500
width = 1500

mHeight, mWidth :: Zoom -> (Exp Float, Exp Float)
mHeight z = (-2/z, 2/z)
mWidth z = (-2/z, 2/z)

maxIters :: Exp Int32
maxIters = lift (255 :: Int32)

runJulia :: IterFun
         -> Acc (Scalar Float) --time
         -> Acc (Scalar Float) --zoom
         -> Acc (Scalar Float) --speed
         -> Acc (Array DIM2 Word32)
runJulia f (the -> time) (the -> zoom) (the -> speed) =
  (toWord32. colorResult . iterateJulia f time speed) $ startingSet zoom

startingSet :: Zoom -> Acc (Array DIM2 IComplex)
startingSet zoom = A.generate (A.constant (Z :. height :. width)) calcSingle
  where
    calcSingle :: Exp DIM2 -> Exp IComplex
    calcSingle (unlift -> Z :. (y :: Exp Int) :. (x :: Exp Int)) =
      let (x1, x2) = mWidth zoom
          (y1, y2) = mHeight zoom
          x' = A.fromIntegral x
          y' = A.fromIntegral y
          width' = A.fromIntegral $ lift width
          height' = A.fromIntegral $ lift height
          -- zx = (x1) + (x'/width) * (P.abs x1 + x2)
          zx = x1 + (x'/width')  * (abs x1 + x2)
          zy = y1 + (y'/height') * (abs y1 + y2)
      in  lift $ zx :+ zy

unpackComplex :: (Elt a, Elt (Complex a)) => Exp (Complex a) -> (Exp a, Exp a)
unpackComplex z = (real z, imag z)

iterateJulia :: IterFun
             -> Exp Float -- time
             -> Exp Float -- speed
             -> Acc (Array DIM2 IComplex)
             -> Acc (Array DIM2 Int32)
iterateJulia f time speed = A.map $ (snd . iter . mkTup)
  where
    mkTup :: Exp IComplex -> Exp (IComplex, Int32)
    mkTup x = lift $ (x, expZero)
      where
        expZero :: Exp Int32
        expZero = 0

    unTup :: Exp (IComplex, Int32) -> ((Exp Float, Exp Float), Exp Int32)
    unTup (unlift -> (z, i)) = (unpackComplex z, i)

    iter :: Exp (IComplex, Int32) -> Exp (IComplex, Int32)
    iter = while inBound iterF

    inBound :: Exp (IComplex, Int32) -> Exp Bool
    inBound (unTup -> ((zx, zy), i)) = i < maxIters && (zx * zx) + (zy * zy) < 4

    iterF :: Exp (IComplex, Int32) -> Exp (IComplex, Int32)
    iterF (unTup -> ((zx, zy), i)) =
      let z' = f time speed (zx, zy) :: Exp IComplex
       in
        lift (z', i+1)

    -- iterF :: Exp (IComplex, Int32) -> Exp (IComplex, Int32)
    -- iterF (unTup -> ((zx, zy), i)) =
    --   let (cx, cy) = cvals :: (Exp Float, Exp Float)
    --       zx' = zx * zx - zy * zy + cx :: Exp Float
    --       zy' = 2 * zx * zy  + cy :: Exp Float
    --
    --       z' = lift $ zx' :+ zy' :: Exp IComplex
    --    in lift $ (z', i + 1)

colorResult :: Acc (Array DIM2 Int32) -> Acc (Array DIM2 Colour)
colorResult = A.map color
  where
    color :: Exp Int32 -> Exp Colour
    color i = cond (i A.== maxBound) black $ colorPick (i `mod` 16)
      where
        black = rgb 100 100 100

        colorPick x = caseof
          x
          [ ((A.== 0),  rgb (66/255)  (30/255)  (15/255))
          , ((A.== 1),  rgb (25/255)  (7/255)   (26/255))
          , ((A.== 2),  rgb (9/255)   (1/255)   (47/255))
          , ((A.== 3),  rgb (4/255)   (4/255)   (73/255))
          , ((A.== 4),  rgb (0/255)   (7/255)   (100/255))
          , ((A.== 5),  rgb (12/255)  (44/255)  (138/255))
          , ((A.== 6),  rgb (24/255)  (82/255)  (177/255))
          , ((A.== 7),  rgb (57/255)  (125/255) (209/255))
          , ((A.== 8),  rgb (134/255) (181/255) (229/255))
          , ((A.== 9),  rgb (211/255) (236/255) (248/255))
          , ((A.== 10), rgb (241/255) (233/255) (191/255))
          , ((A.== 11), rgb (248/255) (201/255) (95/255))
          , ((A.== 12), rgb (255/255) (170/255) (0/255))
          , ((A.== 13), rgb (204/255) (128/255) (0/255))
          , ((A.== 14), rgb (153/255) (87/255)  (0/255))
          , ((A.== 15), rgb (106/255) (52/255)  (3/255))
          ]
          black

toWord32 :: Acc (Array DIM2 Colour) -> Acc (Array DIM2 Word32)
toWord32 = A.map packRGB
