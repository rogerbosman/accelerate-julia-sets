module IterFuns where

import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A

import           Lib

divFactor :: Exp Float
divFactor = 5

iterQuad :: Float -> Exp Float -> (Exp Float, Exp Float) -> Exp IComplex
iterQuad c time (zx, zy) =
  let c' = mkPolar (lift c) (time / divFactor)
      cx = real c'
      cy = imag c'

      zx' = zx * zx - zy * zy + cx :: Exp Float
      zy' = 2 * zx * zy  + cy :: Exp Float
   in lift $ zx' :+ zy'
