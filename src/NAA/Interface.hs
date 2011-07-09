module NAA.Interface (UserInterface(..)) where

import NAA.Data
import NAA.State

-- We don't want to be mixing and matching these events, but this kind of
-- structure allows it.
data UserInterface = UserInterface {
  initialise    :: GameState -> IO (),
  onDisplayGameState :: GameState -> IO (),
  onDisplayBoardState :: BoardState -> IO (),
  onPlayersTurn :: Player -> GameState -> IO Move,
  onGameDraw    :: IO (),
  onGameWin     :: Player -> IO (),
  onGameLose    :: Player -> IO ()
}
