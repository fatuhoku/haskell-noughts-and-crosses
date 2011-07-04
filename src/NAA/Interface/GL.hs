module NAA.Interface.GL where

import NAA.Interface
import NAA.Data
import NAA.Logic

glInterface = UserInterface {
  initialise = initialiseGL,
  onDisplayGameState = draw,
  onPlayersTurn = ???,
  onGameDraw = ???,
  onGameWin  = ???,
  onGameLose = ???
}

-- Handler for when the window is reshaped
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s) -- for carrying out digital zoom, kind of.
  postRedisplay Nothing

-- Initialises the GL window and system
initialiseGL = do
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
  where
    winSize = Size 800 600

    aspectRatio :: Size -> GLdouble
    aspectRatio (Size w h)
      | h /= 0    = fromIntegral w / fromIntegral h
      | otherwise = error "Zero height"

-- How do we render a board? We render each of the cells individually, in the
-- right places, transforming OpenGL matrix as required.
draw gs@(GameState {theBoard=board}) = undefined


