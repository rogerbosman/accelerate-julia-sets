module Main where




import           Data.Array.Accelerate                  (Array, DIM1, DIM2, Elt,
                                                         Scalar, Word32)
import           Data.Array.Accelerate                  as A hiding (unit)
import           Data.Array.Accelerate.Data.Colour.RGB  as A
import           Data.Array.Accelerate.LLVM.PTX         as A
import           Graphics.Gloss.Accelerate.Data.Picture as A

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game       as G
import           Graphics.Gloss.Interface.IO.Game       as G

import           Prelude                                as P hiding (fst, snd,
                                                              (&&), (<), (==),
                                                              (>))

import           IterFuns
import           Lib

data World = World Float

main :: IO ()
main = G.playIO (InWindow "Julia set simulation" (1500, 1500) (0, 0))
                black
                60
                (World 0)
                makePicture
                eventHandler
                timeHandler
  where
    eventHandler :: Event -> World -> IO World
    eventHandler _ w = return w

    timeHandler :: Float -> World -> IO World
    timeHandler delta (World t) = do
      print t
      return $ World (t+delta)

makePicture :: World -> IO Picture
makePicture (World time) = return $ A.bitmapOfArray (juliaArray (unit time)) False

unit :: Elt a => a -> Scalar a
unit a = A.fromList A.Z [a]


-- juliaArray :: Int
-- juliaArray :: Array DIM2 Word32
juliaArray :: Scalar Float -> Array DIM2 Word32
juliaArray = run1 $ runJulia $ iterQuad 0.7885
