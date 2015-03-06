module NAA.Interface (UserInterface(..)) where

import NAA.Data
import NAA.State

-- | Noughts and Arrs permit the use of different user interfaces (UIs). This
-- concept corresponds with both the controller and the view part of the MVC
-- model.
--
-- We first manage the application state (AppState) for the different kinds of UI.
-- (What is the application state good for?)
-- 
-- The game state, see the Controller as the main loop: it has references to the Model
-- (the game state) by the State held in NoughtsAndC
--
-- Usually, UIs need to keep some state. This can be for animation, timers,
-- flags, and other oddities. GLFW callbacks require somewhere to buffer the
-- input events when waitEvents is called even though
-- the user guide seems to suggest that there is a separate user thread hanging
-- around when it comes to handling events. GLFW doesn't work well with
-- other threads, so let's not make things more complicated than they need to
-- be: we waitForEvents on GLFW only when events are required.
--
-- The initialisation of the user interface essentially creates a new set of
-- UIState variables (UIState) that the controller. This should happen only
-- after the game has begun, but after the application has begun.
--
-- The view, when given a UIState, then decides how 
--
-- So, overall:
--  - Model is the GameState along with an AI procedure that the controller may
--    ask "So, how are you going to move?"
--  - The controller decides how to update the UIState 
--
--
-- Let's think of all the events that a GLFW window will require:
--  - initialisation - initialise GLFW and create a window
--  - prompt for user input (first time)   - to obtain a valid move from the user
--  - prompt for user input (second times) - whenver the player makes a mistake
--  - a move is made - we update the display with the new piece and updated game state
--  - there is a draw, or somebody wins - this marks the end of a game, and a
--                                        prompt to play once more.
--
-- onInitialise :: IO () 
--   Called at the beginning of the application.
--
-- onGameStart :: GameState -> IO ()
--   Called at the beginning of a new game, with the given initial game state.
--
-- onRetrieveMove :: Player -> GameState -> IO Move,
--   Called whenever it is a player's turn to move.
--   The retrieve move is returned in the IO Monad.
--   We call waitOnEvents here, and try and handle them.
--
-- onInvalidMove :: Move -> IO ()
--   When the user supplied move is not valid, we call this.
--   We need to record that 
--
-- onPlayerMove :: Move -> GameState -> IO (),
--   Called whenever a player make a move in the game. The move made, and the
--   new game state is given. Typically this displays the new move.
--   
-- onGameEnd :: IO (),
--   Called whenever the game can be declared as a draw, or when a player
--   wins.
--
-- onTerminate :: IO (),
--   Called when the application is closing.
--
data UserInterface = UserInterface {
  onInitialise   :: IO (),
  onGameStart    :: GameState -> IO (),
  onRetrieveMove :: Player -> GameState -> IO Move,
  onInvalidMove  :: Move -> IO (),
  onPlayerMove   :: Move -> GameState -> IO (),
  onGameEnd      :: GameState -> BoardJudgement -> IO (),
  onTerminate    :: IO () 
}
