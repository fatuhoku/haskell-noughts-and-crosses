module NAA.Loop (runNoughtsAndArrs) where

import NAA.AI
import NAA.Logic
import NAA.State
import NAA.Data hiding (player)
import NAA.Interface
import NAA.Interface.CLI
import Control.Monad.Trans
import Control.Monad.State.Lazy

type NoughtsAndArrs a = StateT GameState IO a

-- This is the loop that runs with state!
--
-- The player is shown the board state at every turn. 
-- 1) Repeatedly show prompt, asking for character input of the form (Int,Int)
--    If the parser fails, we ask again.
-- 2) If this is okay, add it into the list of guesses.
-- 3) If there were matches (elem) then do not decrement lives.
-- 4) Check whether the player has won.
--
-- TODO abstract from this to provide
--  a) a curses interface
--  b) a GL interface
--
runNoughtsAndArrs :: UserInterface -> NoughtsAndArrs ()
runNoughtsAndArrs ui = do

  -- Read state and show the game status
  gs@GameState{human=thePlayer,computer=theComputer} <- get
  gs' <- withGameState gs $ \gs -> do
    onDisplayBoardState ui (boardState gs)
    let bs = boardState gs
    let player = turn bs
    let performTurn = if thePlayer == player
                      then onPlayersTurn ui thePlayer
                      else unbeatableAI
    move <- performTurn gs    -- obtain a move from the AI or player
    -- Put the new nought or cross into the state
    let bs' = bs `apply` move
    let gs' = gs {boardState=bs'}
    return gs'
  put gs'

  case judge (board $ boardState gs') of
    Just result -> liftIO $ do
      onDisplayBoardState ui (boardState gs')
      case result of
        Draw      -> onGameDraw ui
        Win p     -> if p == thePlayer
                     then onGameWin ui thePlayer
                     else onGameLose ui thePlayer
        Invalid i -> error . show $ Invalid i
    Nothing -> runNoughtsAndArrs ui           -- neither won or lost. Continue.
  where
    withGameState gs mf = liftIO (mf gs)
