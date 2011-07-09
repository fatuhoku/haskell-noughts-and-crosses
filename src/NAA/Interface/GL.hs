module NAA.Interface.GL where

import Data.Tuple.HT
import qualified Data.Stream as S
import Data.Array.Diff
import Graphics.Rendering.OpenGL hiding (R)
import Graphics.UI.GLUT hiding (R)
import NAA.Interface
import NAA.Data
import NAA.Logic
import NAA.State

glInterface = UserInterface {
  initialise = const initialiseGL,
  onDisplayGameState = drawGameState,
  onDisplayBoardState = drawBoardState,
  onPlayersTurn = \p gs -> return (R,(0,0)),
  onGameDraw = return (), -- TODO 
  onGameWin  = const $ return (), -- TODO 
  onGameLose = const $ return ()  -- TODO
}

-- Initialises the GL window and system
initialiseGL :: IO ()
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
  ortho2D (-1.6) (1.6) (0.0) (2.0)
  matrixMode         $= Modelview 0
  loadIdentity
  reshapeCallback    $= Just reshape
  where
    winSize = Size 800 600

    aspectRatio :: Size -> GLdouble
    aspectRatio (Size w h)
      | h /= 0    = fromIntegral w / fromIntegral h
      | otherwise = error "Zero height"

    reshape s@(Size w h) = do
      viewport $= (Position 0 0,s)
      postRedisplay Nothing

-- How do we render a board? We render each of the cells individually, in the
-- right places, transforming OpenGL matrix as required.
drawGameState :: GameState -> IO ()
drawGameState gs@(GameState {boardState=bs,wins=w,losses=l}) = do
  drawBoardState bs

drawBoardState :: BoardState -> IO ()
drawBoardState bs@(BoardState {board=brd,turn=currentTurn}) = do
  drawBoard brd

drawBoard :: Board -> IO ()
drawBoard (Board brd) = do
  renderPrimitive Lines $ do
    mapM_ vertex gridVerts
    -- TODO draw the nought and draw the crosses
  where
  ((minM,minN),(maxM,maxN)) = bounds brd
  maxM' = fromIntegral maxM :: GLdouble
  maxN' = fromIntegral maxN :: GLdouble
  gridVerts = [Vertex3 x y 0 | x <- [0,(1/(maxN'+1))..1], y <- [0,1]]    -- Vertical lines
              ++ [Vertex3 x y 0 | x <- [0,1], y <- [0,(1/(maxM'+1))..1]] -- Horizontal lines
 
zAxis = Vector3 0 0 (-1::GLfloat)
-- Simply draw a grid given a Board. Assume the grid is to be drawn on a unit
-- square. Lines need to be produced.
--
--
-- Draws a 'cross' in a unit square in noughts and crosses.
cross = do
  scale 0.8 0.8 (0.8 :: GLfloat)
  rotate degs45 zAxis
  renderPrimitive LineLoop $ mapM_ vertex $
    [Vertex3 b   0   0,
     Vertex3 ba  0   0,
     Vertex3 ba  b   0,
     Vertex3 bab b   0,
     Vertex3 bab ba  0,
     Vertex3 ba  ba  0,
     Vertex3 ba  bab 0,
     Vertex3 b   bab 0,
     Vertex3 b   ba 0,
     Vertex3 0   ba 0,
     Vertex3 0   b 0,
     Vertex3 b b 0]
  where
    a = 0.125 :: GLfloat
    b = 0.375 :: GLfloat
    ba  = b+a
    bab = b+a+b
    degs45 = pi/4

nought = do
  scale 0.8 0.8 (0.8 :: GLfloat)
  renderPrimitive TriangleFan $ do
    mapM_ vertex noughtVerts
  where
    dAngle = pi/12
    sr = 0.7

    noughtVerts = S.take (lenInner + lenOuter) $ S.interleave sInner sOuter

    sInner = S.fromList $ cycle innerVerts
    sOuter = S.fromList $ cycle outerVerts

    innerVerts = do
      angle <- [0.0,dAngle..2*pi-0.0001]
      return $ Vertex3 (sin angle) (cos angle) (0::GLfloat)

    outerVerts = do
      angle <- [dAngle,1.5*dAngle..2*pi-0.0001] 
      return $ Vertex3 (sr*(sin angle)) (sr*(cos angle)) (0::GLfloat)

    lenInner = length innerVerts
    lenOuter = length outerVerts
