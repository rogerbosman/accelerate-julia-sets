module Main where

import           Graphics.Gloss
import           Lib

import           Data.Array.Accelerate                  (Array, DIM1, DIM2, Elt,
                                                         Scalar, Word32)
import qualified Data.Array.Accelerate                  as A hiding (round)
import           Data.Array.Accelerate.Data.Colour.RGB  as A
import           Data.Array.Accelerate.LLVM.PTX         as A
import           Graphics.Gloss.Accelerate.Data.Picture as A

import           Graphics.Gloss.Interface.IO.Game       as G
import           Graphics.Gloss.Interface.IO.Game       as G

import           Prelude                                as P hiding (fst, snd,
                                                              (&&), (<), (==),
                                                              (>))


data World = World Int

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
    timeHandler _ w = return w

makePicture :: World -> IO Picture
makePicture i = return $ A.bitmapOfArray juliaArray False

juliaArray :: Array DIM2 Word32
juliaArray = run $ runJulia undefined
