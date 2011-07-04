module NAA.Logic (GameState(..),createBoard3x3,judge,apply) where

-- import Prelude hiding (map)
import qualified Data.Vector as V
import Data.Array.Diff
import Data.Maybe
import Debug.Trace

import NAA.Data

type GameResult = BoardJudgement

-- R always starts.
data GameState = GameState {
  theBoard :: Board,           -- the game board.
  turn     :: Turn,            -- whose turn is it?
  wins     :: Int,             -- the number of player wins
  losses   :: Int              -- the number of player losses
}

instance Show GameState where
  show (GameState {theBoard=board,turn=theTurn}) =
    "It's " ++ show theTurn ++ "'s turn.\n\n" ++ show board ++ "\n"

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
  
    getCells  = map (brd!) -- transforms indicices for a row, column or diagonal
    rowIdxs i = [(i,j) | j <- [minN..maxN]] -- RowWin i Player :: [[Idx2D]]
    colIdxs j = [(i,j) | i <- [minM..maxM]] -- ColWin j Player :: [[Idx2D]]
    diagIdxsMajor = [(i,i) | i <- [minM..maxM]] -- SE direction
    diagIdxsMinor = [(i,j) | i <- [minM..maxM], j <- [maxN,maxN-1..minN]] -- NE direction
    ((minM,minN),(maxM,maxN)) = bounds brd

judgeRCD :: (Player -> RCDJudgement) -> RowColDiag -> Maybe RCDJudgement
judgeRCD _ [] = error "judgeRCD: Empty row, column or diagonal." 
judgeRCD f (c:cs) = case c of
                      Empty   -> Nothing
                      Piece p -> if all (==c) cs then Just (f p) else Nothing

-- Applies a move onto the board. This formalises the setting of a cell on the
-- board. Use as infix operation.
-- DOES NOT DO SANITY CHECKS! Quite unsafe indeed.
apply :: Board -> Move -> Board
apply (Board brd) (Move m) = Board $ brd // [(coord,Piece player)]
  where
    (player,coord) = m

judgeRow  i = judgeRCD (RowWin i)
judgeCol  j = judgeRCD (ColWin j)
judgeDiag d = judgeRCD (DiagWin d)

ftrace = flip trace

createBoard3x3 :: [Cell] -> Board
createBoard3x3 = Board . listArray ((0,0),(2,2))
