module NAA.Interface.GL where

import Data.Tuple.HT
import qualified Data.Stream as S
import Data.Array.Diff
import Graphics.Rendering.OpenGL hiding (R)
import Graphics.UI.GLFW
import Control.Concurrent
import NAA.Interface
import NAA.Data
import NAA.Logic
import NAA.State

glInterface = UserInterface {
  onInitialise = const initialiseGL,
  onDisplayGameState = drawGameState,
  onDisplayBoardState = drawBoardState,
  onPlayersTurn = \p gs -> return (R,(0,0)), -- TODO: this is an invalid strategy...
  onGameEnd   = \gs j   -> return (),        -- TODO
  onTerminate = terminate               -- Terminates GLFW
}

-- This is the code that handles onGameEnd.
-- case result of
--         Draw      -> onGameDraw ui
--         Win p     -> if p == thePlayer
--                      then onGameWin ui thePlayer
--                      else onGameLose ui thePlayer
--         Invalid i -> error . show $ Invalid i

-- Initialise using GLFW for creating our Window.
-- We set the window size to 800 by 600.
-- Refreshing GLFW requires a mainloop to stay responsive to the user: to events
-- such as resizing of the Window. 
-- This means that we must spark off a UI thread using forkIO to allow
-- GLFW to poll for inputs. The game loop needs to wait on the availability of
-- an MVar, which the UI thread will populate with the user's intended move.
initialiseGL :: IO ()
initialiseGL = do
  initSuccess <- initialize 
  if initSuccess then return () else fail "initialiseGL: failed initialising GLFW"
  windSuccess <- openWindow $ defaultDisplayOptions
                                { displayOptions_width  = 600
                                , displayOptions_height = 600
                                }
  if windSuccess then return () else fail "initialiseGL: could not open window"
  setWindowTitle "Noughts and R's"
  setWindowPosition 200 200
  setWindowSizeCallback onResize
  setWindowCloseCallback onWindowClose
  setKeyCallback onKeyPressed
  setMouseButtonCallback onMouseButton
  setMousePositionCallback onMousePosition
  setMouseWheelCallback onMouseWheel

  -- At this point, we have a window! We can spark a thread off to do its
  -- drawing!

  forkIO uiThread    -- TODO I might want this ThreadId at some point
  return ()
  where
    onResize w h       = return ()
    onKeyPressed key b = return ()
    onMousePosition x y   = return ()
    onMouseButton msbtn b = return ()
    onMouseWheel n     = return ()
    onWindowClose      = return True

uiThread :: IO ()
uiThread = do
  -- initialDisplayMode $= [DoubleBuffered,RGBMode]
  -- initialWindowSize  $= winSize
  clear [ColorBuffer]
  viewport           $= ((Position 0 0),winSize)
  matrixMode         $= Projection
  loadIdentity
  let asp = aspectRatio winSize
  ortho2D (-1.6) (1.6) (0.0) (2.0)
  matrixMode         $= Modelview 0
  loadIdentity
  swapBuffers
  where
    winSize = Size 800 600

    aspectRatio :: Size -> GLdouble
    aspectRatio (Size w h)
      | h /= 0    = fromIntegral w / fromIntegral h
      | otherwise = error "Zero height"


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
