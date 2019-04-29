{-# LANGUAGE RecordWildCards #-}

module Main where

import           System.Exit

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
import           World

main :: IO ()
main = G.playIO (InWindow "Julia set simulation" (1500, 1500) (0, 0))
                black
                60
                starterWorld
                makePicture
                eventHandler
                timeHandler
  where
    starterWorld = World { time = 0
                         , zoom = 1
                         , speed = 0.2 --1 is already very fast
                         }

    eventHandler :: Event -> World -> IO World
    eventHandler e w@World{..} = case e of
      EventKey (SpecialKey KeyEsc)   Down _ _ -> exitSuccess
      EventKey (SpecialKey KeySpace) Down _ _ -> exitSuccess
      EventKey (Char c) Down _ _ -> case c of
        'w' -> return w{zoom = zoom * 1.25} -- zoom in  by 25%
        's' -> return w{zoom = zoom * 0.8 } -- zoom out by 25%


        'q' -> return w{speed = speed * 0.8 } -- slow  down by 25%
        'e' -> return w{speed = speed * 1.25} -- speed up   by 25%

        _   -> return w
      _ -> return w

    timeHandler :: Float -> World -> IO World
    timeHandler delta w@World{..} = do
      return $ w{time = time + (delta * speed)}

makePicture :: World -> IO Picture
makePicture World{..} = return $ A.bitmapOfArray arr False
  where
    arr = juliaArray (unit time)
                     (unit zoom)
                     (unit speed)

unit :: Elt a => a -> Scalar a
unit a = A.fromList A.Z [a]


-- juliaArray :: Int
-- juliaArray :: Array DIM2 Word32
juliaArray :: Scalar Float -- time
           -> Scalar Float -- zoom
           -> Scalar Float -- speed
           -> Array DIM2 Word32
juliaArray = runN $ runJulia $ iterQuad 0.7885
