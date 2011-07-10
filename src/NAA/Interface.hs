module NAA.Interface (UserInterface(..)) where

import NAA.Data
import NAA.State

-- We don't want to be mixing and matching these events, but this kind of
-- structure allows it.
---- This happens when the game wishes to terminate and close the window.
--
-- onInitialise
--   Called before the game actually starts.
--
-- onDisplayGameState :: GameState -> IO (),
--   Called whenever the GameState needs to be drawn completely, because
--   it has been updated somehow.
--   TODO how do we find deltas between two structures?
--
-- onDisplayBoardState :: BoardState -> IO (),
--   Called whenever the BoardState is updated within a game. This has certain
--   overlap with onDisplayGameState, because drawing the GameState typically
--   means drawing the BoardState too.
--
-- onPlayersTurn :: Player -> GameState -> IO Move,
--   Called whenever it is a player's turn. The current GameState is also given.
--
-- onGameDraw    :: IO (),
--   Called whenever the game can be declared as a draw.
--
-- onGameWin     :: Player -> IO (),
--   Called whenever a player wins. The winning player is provided.
--   TOOD Provide winning judgements as well.
--
-- onGameLose    :: Player -> IO ()
--   Called whenever a player wins. The winning player is provided.
--   TOOD Provide winning judgements as well.
--
-- onTerminate   :: Player -> IO () 
data UserInterface = UserInterface {
  onInitialise  :: GameState -> IO (),
  onDisplayGameState :: GameState -> IO (),
  onDisplayBoardState :: BoardState -> IO (),
  onPlayersTurn :: Player -> GameState -> IO Move,
  onGameEnd     :: GameState -> BoardJudgement -> IO (),
  onTerminate   :: IO () 
}
