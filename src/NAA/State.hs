module NAA.State (BoardState(..),
                  GameState(..),
                  blankBoardState) where

import NAA.Data

data GameState = GameState {
  boardState :: BoardState,    -- the game board state
  wins     :: Int,             -- the number of player wins
  losses   :: Int,             -- the number of player losses
  -- Immutables (the idea is that they aren't set, anyway)
  human    :: Player,
  computer :: Player
}

data BoardState = BoardState {
  board    :: Board,           -- what is the state of the board at the moment?
  turn     :: Turn             -- whose turn is it next?
}

blankBoardState :: Turn -> BoardState
blankBoardState t = BoardState blankBoard3x3 t
  where
    blankBoard3x3 = boardFromList 3 $ replicate 9 Empty

-- When do we show the game  state?
instance Show GameState where
  show (GameState{}) = "*** show GameState ***"

instance Show BoardState where
  show (BoardState {board=theBoard,turn=theTurn}) =
    turnIndicator ++ "\n\n" ++ show theBoard ++ "\n"
    where
      turnIndicator = ""
      -- turnIndicator = "It's " ++ show theTurn ++ "'s turn."

