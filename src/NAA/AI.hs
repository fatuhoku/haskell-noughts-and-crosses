module NAA.AI where

import Data.Array.Diff
import Data.List.Extras.Argmax

import NAA.Logic
import NAA.Data

-- The unbeatable AI
-------------------------
-- Based on a simple mini-max algorithm. 
-- Scoring is based on the number of ways that a given a particular
unbeatableAI :: Player -> GameState -> IO Move
unbeatableAI p gs@(GameState {theBoard=board}) =
  return $ argmax (minimax p p . apply board) $ validMoves board p

-- Minimax for player. Generate next moves dynamically, why not.
-- We can inspect whose turn it is because GameState is included.
minimax :: Turn -> Player -> Board -> Int
minimax turn maxPlayer board =
  -- When it's max player, we consider minima of the opponent.
  let op = if turn == maxPlayer then minimum else maximum in
  case judge board of
    Just (Win p') -> if maxPlayer == p' then (1) else (-1)
    Just Draw     -> 0
    Nothing       -> op $ map (minimax (other turn) maxPlayer) nextPly
    _  -> error "minimax: reached Invalid board configuration"
  where
    nextPly = zipWith apply (repeat board) $ validMoves board turn

validMoves :: Board -> Player -> [Move]
validMoves board@(Board brd) p = map Move $ zip (repeat p) (validMoves' board)

-- Returns Cell locations where it is valid to put A PIECE.
-- This is essentially all the empty cells, except on completed boards,
-- where the cells are not included. To prevent judging a board too many
-- Precondition: the board is valid.
validMoves' :: Board -> [Idx2D]
validMoves' (Board brd) = map fst $ filter ((==Empty) . snd) $ assocs brd


-- Our heuristic is:
-- The number of possible ways I can win - the number of possible ways the
-- opponent can win.
-- Surely we head towards victory in this way.
-- TODO 
