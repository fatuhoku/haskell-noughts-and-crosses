module NAA.AI where

import Data.Array.Diff
import Data.List.Extras.Argmax

import NAA.Data
import NAA.Logic
import NAA.State

type Score = Integer

-- The unbeatable AI
-------------------------
-- Based on a simple mini-max algorithm. 
-- Scoring is based on the number of ways that a given a particular
unbeatableAI :: GameState -> IO Move
unbeatableAI gs@(GameState {computer=me,boardState=bs}) =
  return $ argmax (score me . apply bs) $ validMoves bs 

-- Minimax
-- --------------
-- We don't know who our opponent is, but we assume the most pessimistic case:
-- that he is very able, and has the capability of selecting (consistently) a
-- move that would minimise the score for our player.
--

-- nextPly computes all of the boards that can be reached from the current board
-- state. If the Board is at an endgame, then no next board states are created.
nextPly :: BoardState -> [BoardState]
nextPly bs = zipWith apply (repeat bs) (validMoves bs)

-- Returns all the valid moves from the given BoardState.
validMoves :: BoardState -> [Move]
validMoves (BoardState {board=brd,turn=plyr}) = zip (repeat plyr) (emptyCells brd)
  where
    emptyCells :: Board -> [Idx2D]
    emptyCells (Board brd) = map fst $ filter ((==Empty) . snd) $ assocs brd


-- The 'score' of some Board b is simply the sum of 'scores' of child boards.
-- It is a heuristic based on the immediate capability to win (scores 10^9), and
-- if not, the indirect capability to win. Positive integers represent the
-- possibility of winning and the negative integers represent the possibility of
-- the opponent winning.
--
-- The score of a winning board is MAXSCORE; the score of a losing board is
-- MINSCORE.
-- The score of a board that has no endgame judgement is the sum of the
-- of the scores of the boards in the next ply NEGATED and NORMALISED by /9.
score :: Player -> BoardState -> Score
score p bs@(BoardState {board=theBoard,turn=p'}) =
  case judge theBoard of
    Nothing       -> (`div` 9) . negate . sum $ map (score (other p)) (nextPly bs)
    Just Draw     -> 0
    Just (Win p') -> if p == p' then maxScore else -maxScore
    Just (Invalid _) -> error "score: Invalid board configuration"
  where
    maxScore = 9^9

-- Let's use the FGL to construct a graph of moves. Then we could write an
-- inductive algorithm to find the branch with the most number of wins or
-- something.
