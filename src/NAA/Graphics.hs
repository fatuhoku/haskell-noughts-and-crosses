module NAA.Graphics where

import NAA.Data
import NAA.Logic

-- Ratio between width and height.
aspectRatio :: Size -> GLdouble
aspectRatio (Size w h)
  | h /= 0    = fromIntegral w / fromIntegral h
  | otherwise = error "Zero height"

winSize = Size 800 600

initGL = do
  (progName,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,RGBMode]
  initialWindowSize  $= winSize
  createWindow "Noughts and R's"
  clear [ColorBuffer]
  viewport           $= ((Position 0 0),winSize)
  matrixMode         $= Projection
  loadIdentity
  let asp = aspectRatio winSize
  orth2D (-1.6) (1.6) (0.0) (2.0)
  matrixMode         $= Modelview 0
  loadIdentity
  reshapeCallback    $= Just reshape

reshape = undefined
