module NAA.Logic (GameState(..),judge,apply,isValidIn) where

-- import Prelude hiding (map)
import qualified Data.Vector as V
import Data.Array.Diff
import Data.Maybe
import Debug.Trace

import NAA.Data
import NAA.State

-- Passes judgement over a game board.
-- With a 2D array this is easy:
--  - check cols from the min to max bounds
--  - check rows from the min to max bounds
--  - check diagonals from min to max bounds
--
-- Fix row i. the indicies for each element is simply:
--    [(i,j) | j <- [minN..maxN]]
--
-- Fix col j. the indicies for each element is simply:
--    [(i,j) | i <- [minM..maxM]]
--
-- The indicies for the two diagonals is simply:
--    [(i,i) | i <- [minM..maxM]]
--    [(i,j) | i <- [minM..maxM], j <- [maxN,maxN-1..minN]]
--
-- After passing judgement for all rows, columns or diagonals (failing when no
-- player wins) a board judgement can be passed.
--
-- Just Tie | Win | Invalid signify the END OF THE GAME.
-- Nothing signifies that the game may continue.
--
judge :: Board -> Maybe BoardJudgement
judge b@(Board brd) =
  case catMaybes $ rowJs ++ colJs ++ diagJs of
    []                -> if full brd then Just Draw else Nothing
    judgements@(j:js) ->
      let p = player j
      in Just $ if all ((==p) . player) js
                then Win p
                else Invalid judgements
  where
    full = all (/=Empty) . elems
    rowJs = do
      i <- [minM..maxM]
      return . judgeRow i $ getCells (rowIdxs i)

    colJs = do
      j <- [minN..maxN]
      return . judgeCol j $ getCells (colIdxs j)

    diagJs = [judgeDiag Major $ getCells $ diagIdxsMajor,
              judgeDiag Minor $ getCells $ diagIdxsMinor]
  
    getCells :: [Idx2D] -> [Cell]
    getCells  = map (brd!)
    rowIdxs i = [(i,j) | j <- [minN..maxN]]
    colIdxs j = [(i,j) | i <- [minM..maxM]]
    diagIdxsMajor = [(i,i) | i <- [minM..maxM]]
    diagIdxsMinor = [(i,maxM-i) | i <- [minM..maxM]]
    ((minM,minN),(maxM,maxN)) = bounds brd

judgeRow  i = judgeRCD (RowWin i)
judgeCol  j = judgeRCD (ColWin j)
judgeDiag d = judgeRCD (DiagWin d)

judgeRCD :: (Player -> RCDJudgement) -> RowColDiag -> Maybe RCDJudgement
judgeRCD _ [] = error "judgeRCD: Empty row, column or diagonal." 
judgeRCD f (c:cs) = case c of
                      Empty   -> Nothing
                      Piece p -> if all (==c) cs then Just (f p) else Nothing

-- Applies a move onto the board. This formalises the setting of a cell on the
-- board. Use as infix operation.
apply :: BoardState -> Move -> BoardState
apply bs@(BoardState {board=(Board brd),turn=p'}) (p,coord)
  | p == p'   = bs {board=Board $ brd // [(coord,Piece p)],turn=other p}
  | otherwise = error $ "apply: player " ++ show 
                p ++ " tried to move when it's actually "
                ++ show p' ++"'s turn!"

-- isValidIn takes a move and returns whether it is a valid move in the board
-- state.
-- A valid move is when the turns agree
isValidIn :: Move -> BoardState -> Bool
isValidIn (player,(i,j)) (BoardState {board=Board brd,turn=theTurn}) =
  let ((minM,minN),(maxM,maxN)) = bounds brd in
  player == theTurn &&
  minM <= i && i <= maxM &&
  minN <= j && j <= maxN &&
  brd ! (i,j) == Empty

ftrace = flip trace
