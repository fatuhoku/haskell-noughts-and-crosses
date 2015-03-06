module NAA.Loop (runNoughtsAndArrs) where

import NAA.AI
import NAA.Logic
import NAA.State
import NAA.Data hiding (player)
import NAA.Interface
import NAA.Interface.CLI
import Control.Monad.Trans
import Control.Monad.State.Lazy

-- So not only do we need to keep running the GameState over IO but we probably
-- want some scene state information.
type NoughtsAndArrs a = StateT GameState IO a

-- We start a game, loop as necessary, and end a game.
-- The end of the game is handled by the loop!
runNoughtsAndArrs :: UserInterface -> NoughtsAndArrs ()
runNoughtsAndArrs ui = do
  gs <- get
  liftIO $ onGameStart ui gs
  runGameLoop ui -- >> onGameEnd ui

-- | Loop maintains the liftime of a single game. It carries out a game with
-- they help of an Interface.
--
-- 1) onRetrieveMove. This is a blocking call. It only returns when the player has
--    chosen a move to make. It can be an invalid one, and no input validation
--    is expected.
--    It is the Interface's responsibility to show the game state in a way that
--    a human being can make an informed decision about what
--    move to take. It is also the Interface's responsibility to deal with the
--    necessary IO to retrieve the move's coordinates.
--
-- 2) onInvalidMove. This is a non-blocking call. This notifies the interface
--    that the move given by onRetrieveMove was found to be not valid for the
--    current GameState. This permits the error to be fed back to the player in
--    some form. onRetrieveMove is invoked again.
--
-- 3) onPlayerMove. This is a non-blocking call. This is invoked when a player
--    has made a valid move, and is an opportunity for the Interface to update
--    itself.
--
-- 4) onGameEnd. This is a non-blocking call. This is invoked when the game has
--    reached an end state: either draw, or win.
--
runGameLoop :: UserInterface -> NoughtsAndArrs ()
runGameLoop ui = do

  -- Get the current game state and prompt the user to make a move
  -----------------------------------------------------------------
  gs@GameState{human=plyr,computer=comp} <- get
  gs' <- withGameState gs $ \gs -> do
    let bs@(BoardState {turn=currentTurn}) = boardState gs
    let retrieveMove = if plyr == currentTurn
                       then retrieveMoveFromPlayer plyr
                       else unbeatableAI
    move <- retrieveMove gs    -- obtain a move from the AI or player
    
    -- Compute the upated game state
    ---------------------------------
    let bs' = bs `apply` move
    let gs' = gs {boardState=bs'}
    onPlayerMove ui move gs'
    return gs'

  put gs'

  -- Here, we decide how to continue after the move has been made. 
  -- If it yielded an end-game result then we will notify the interface
  -- Otherwise we continue looping, running the Noughts and Arrs game.
  -----------------------------------------------------------------
  let BoardState {board=brd} = boardState gs'
  case judge brd of
    Just result -> liftIO $ onGameEnd ui gs' result   -- when the game ends with result
    Nothing     -> runGameLoop ui               -- neither won or lost. Continue.

  where
    withGameState gs mf = liftIO (mf gs)

    retrieveMoveFromPlayer :: Player -> GameState -> IO Move
    retrieveMoveFromPlayer plyr gs = msum . repeat $ do
      m <- onRetrieveMove ui plyr gs
      validate m gs
      return m

    validate :: Move -> GameState -> IO (Idx2D)
    validate m (GameState {boardState=bs})
      | m `isValidIn` bs = return $ snd m
      | otherwise = onInvalidMove ui m >> mzero
