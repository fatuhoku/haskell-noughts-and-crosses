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
import System.Exit

glInterface = UserInterface {
  onInitialise = initialiseGL,
  onGameStart  = drawGameState,
  onRetrieveMove = onRetrieveMoveGL, -- TODO: this is an invalid strategy...
  onInvalidMove  = const $ return (),-- with validation we shouldn't get this.
  onPlayerMove   = onPlayerMoveGL,   -- TODO
  onGameEnd   = \gs j   -> return (),-- TODO
  onTerminate = terminate            -- Terminates GLFW
}

-- newtype BoardView = BoardView 

winSize = Size 600 600

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
  createWindow "Noughts and Arrs" winSize
  setupCallbacks

createWindow :: String -> Size -> IO ()
createWindow title (Size w h) = do
  initSuccess <- initialize 
  if initSuccess
    then return ()
    else fail "createWindow: failed initialising GLFW"
  windSuccess <- openWindow $ defaultDisplayOptions
                                { displayOptions_width  = fromIntegral w
                                , displayOptions_height = fromIntegral h
                                }
  if windSuccess
    then return ()
    else fail "initialiseGL: could not open window"
  
  setWindowTitle "Noughts and R's"
  setWindowPosition 200 200          -- This is a decent place to put the window!


setupCallbacks :: IO ()
setupCallbacks = do

  -- Window --
  setWindowCloseCallback onWindowClose
  setWindowSizeCallback onResize
  setWindowRefreshCallback onWindowRefresh

  -- Keyboard --
  setKeyCallback onKeyPressed

  -- Mouse --
  setMouseButtonCallback onMouseButton
  setMousePositionCallback onMousePosition
  setMouseWheelCallback onMouseWheel
  where
    onResize w h       = return ()    -- Don't do anything, as it doesn't matter.
    onKeyPressed key b = putStr $ "Key pressed: " ++ show key ++ "\n"
    onMousePosition x y   = return ()
    onMouseButton msbtn b = return ()
    onMouseWheel n     = return ()
    onWindowRefresh    = return () -- drawGameState
    onWindowClose      = exitSuccess >> return True
    -- We terminate the application prematurely.

-- We draw the game state once. Currently only renders the grid.
drawGameState :: GameState -> IO ()
drawGameState gs = do
  -- initialDisplayMode $= [DoubleBuffered,RGBMode]
  -- initialWindowSize  $= winSize
  clear [ColorBuffer]
  viewport           $= ((Position 0 0),winSize)
  matrixMode         $= Projection
  loadIdentity
  let asp = aspectRatio winSize
  ortho2D (-1.0) (1.0) (-1.0) (1.0)
  matrixMode         $= Modelview 0
  loadIdentity

  -- START DRAWING!!!!
  preservingMatrix $ do
    color $ Color3 1.0 1.0 (1.0::GLfloat)
    grid 0.8
  -- preservingMatrix $ do
  --   color $ Color3 0.0 0.0 (1.0::GLfloat)
  --   nought 0.8
  -- preservingMatrix $ do
  --   color $ Color3 1.0 0.0 (0.0::GLfloat)
  --   cross 0.8
  -- END DRAWING!!!

  swapBuffers
  where
    aspectRatio :: Size -> GLdouble
    aspectRatio (Size w h)
      | h /= 0    = fromIntegral w / fromIntegral h
      | otherwise = error "Zero height"


-- We theorise: how do we return two IO actions create the same moveMvar
do
  moveMVar <- newMVar
  

onRetrieveMoveGL :: MVar Move -> Player GameState -> IO Move

-- Wait for events... but the callback handler must communicate with
-- the callback.
onRetrieveMoveGL :: Player -> GameState -> IO Move
onRetrieveMoveGL p gs = do
  drawGameState gs    -- TODO draw some words to indicate whose turn it is!
  waitEvents          -- This invokes all the callbacks necessary!
                      -- We might expect that an MVar is set somehere here.
  return $ (p,(0,0))

-- Draw the game state with the updated cells.
onPlayerMoveGL :: Move -> GameState -> IO ()
onPlayerMoveGL m gs = do
  drawGameState gs
 


------------- Drawing Functions ----------------
zAxis = Vector3 0 0 (-1::GLfloat)

cross :: GLfloat -> IO ()
cross s = do
  scale s s s
  rotate (45::GLfloat) zAxis
  rect (Vertex2 x1 y0) (Vertex2 x2 y3)
  rect (Vertex2 x0 y1) (Vertex2 x3 y2)
  where
    (x0,x1,x2,x3) = (-1,-b,b,1)
    (y0,y1,y2,y3) = (-1,-b,b,1)
    b = 1/6 :: GLfloat

-- Draws a wireframe with scaling factor
crossW :: GLfloat -> IO ()
crossW s = do
  scale s s s
  rotate (45::GLfloat) zAxis
  renderPrimitive LineLoop $ mapM_ vertex $
    [Vertex3 x0  y1  0,
     Vertex3 x1  y1  0,
     Vertex3 x1  y0  0,
     Vertex3 x2  y0  0,
     Vertex3 x2  y1  0,
     Vertex3 x3  y1  0,
     Vertex3 x3  y2  0,
     Vertex3 x2  y2  0,
     Vertex3 x2  y3  0,
     Vertex3 x1  y3  0,
     Vertex3 x1  y2  0,
     Vertex3 x0  y2  0]
  where
    (x0,x1,x2,x3) = (-1,-b,b,1)
    (y0,y1,y2,y3) = (-1,-b,b,1)
    b = 1/6 :: GLfloat

-- Draw a nought in wireframe. This should include arcs into the centre ring.
-- Outer: [o0,o1,o2,o3,o4,o5,o6,o7]
-- Inner: [i0,i1,i2,i3,i4,i5,i6,i7]
nought :: GLfloat -> IO ()
nought s = do
  scale s s s
  renderPrimitive TriangleStrip $ mapM_ vertex noughtVerts
  where
    dAngle  = pi/20    -- draw 24 point-resolution ring on screen.
    ioRatio = 0.6     -- this is inner-outer ratio

    -- noughtVerts = outerVerts
    noughtVerts = S.take (innerLen + outerLen) $ S.interleave innerS outerS

    innerS = S.fromList $ cycle innerVs
    outerS = S.fromList $ cycle outerVs

    innerAngles = [0.0,dAngle..2*pi-0.0001]

    outerVs = do
      angle <- map (+0.5*dAngle) innerAngles
      return $ Vertex3 (sin angle) (cos angle) (0::GLfloat)

    innerVs = do
      angle <- innerAngles
      return $ Vertex3 (ioRatio*(sin angle)) (ioRatio*(cos angle)) (0::GLfloat)

    innerLen = length innerVs
    outerLen = length outerVs

-- Draws 
grid :: GLfloat -> IO ()
grid s = do
  scale s s s
  hline (-1) (1) (-1/3) stroke
  hline (-1) (1) (1/3) stroke
  vline (-1) (1) (-1/3) stroke
  vline (-1) (1) (1/3) stroke
  where
    stroke = 0.01   -- lines are 6 pixels wide
    -- gridVerts = [Vertex3 x y 0 | x <- [0,(1/(maxN'+1))..1], y <- [0,1]]    -- Vertical lines
    --           ++ [Vertex3 x y 0 | x <- [0,1], y <- [0,(1/(maxM'+1))..1]] -- Horizontal lines
    
-- hline x1 x2 y s draws a line with stroke size s across the screen horizontally
-- at height y
hline :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
hline x1 x2 y s = rect (Vertex2 x1 (y-s)) (Vertex2 x2 (y+s))
  
vline :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
vline y1 y2 x s = rect (Vertex2 (x-s) y1) (Vertex2 (x+s) y2)

