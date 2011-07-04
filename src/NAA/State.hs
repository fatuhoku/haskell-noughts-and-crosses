module NAA.State (GameState(..)) where

import NAA.Data

data GameState = GameState {
  theBoard :: Board,           -- the game board.
  turn     :: Turn,            -- whose turn is it?
  wins     :: Int,             -- the number of player wins
  losses   :: Int,             -- the number of player losses
  -- Immutables (the idea is that they aren't set, anyway)
  human    :: Player,
  computer :: Player
}

instance Show GameState where
  show (GameState {theBoard=board,turn=theTurn}) =
    turnIndicator ++ "\n\n" ++ show board ++ "\n"
    where
      turnIndicator = ""
      -- turnIndicator = "It's " ++ show theTurn ++ "'s turn."

