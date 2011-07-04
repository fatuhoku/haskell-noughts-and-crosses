{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}
module NAA.Data (Board(..),
                 Cell(..),  
                 Player(..),
                 BoardJudgement(..),
                 RCDJudgement(..),
                 DiagDir(..),
                 Move(..),
                 Turn,Idx2D,
                 Row,Col,Diag,RowColDiag,player,other) where

-- import qualified Data.Vector as V
import Data.List
import Data.List.Split
import Data.Array.Diff
import qualified Data.Vector as V

-- A player is either R or O
-- A cell may be empty, or it is occupied by a piece owned by a player
-- A board is a two-dimensional vector of cells
-- A 'judgement' on a board is a summative statement about a row, column or
-- diagonal, which are combined via a Monoid instance to give the judgement of
-- the entire board.
data Player    = R | O                deriving (Show,Eq)
data Cell      = Empty | Piece Player deriving (Eq)
data DiagDir   = Major | Minor
type Idx2D     = (Int,Int)
type Turn      = Player

type Row  = [Cell]
type Col  = [Cell]
type Diag = [Cell]
type RowColDiag  = [Cell]

-- To pass judgement over a board, we pass judgement over rows, columns and
-- diagonals (Maybe RCDJudgement) with Nothing indicating that nothing can be said about
-- the RC or D. The good rows are merged to give the final board result.
-- If incompatible RCDJudgements are found, then an Invalid BoardJudgement will
-- arise. The payload will carry the conflicting judgements: basically, the one
-- that has the players.
-- 
-- Simply find all RCDJudgements; filter for UnWin entries, and ensure that
-- all the players in these entries are 
data RCDJudgement   = RowWin Int Player
                    | ColWin Int Player
                    | DiagWin DiagDir Player

data BoardJudgement = Draw | Win Player | Invalid [RCDJudgement]

newtype Board  = Board (DiffArray Idx2D Cell)
newtype Move = Move (Player,Idx2D)

instance Show Cell where
  show (Piece p) = show p
  show Empty = " "

instance Show BoardJudgement where
  show Draw        = "This game is a draw."
  show (Win p)     = "Player " ++ show p ++ " has won!"
  show (Invalid i) = "Invalid board. The following are not compatible: "
                     ++ (unlines $ map show i)

instance Show RCDJudgement where
  show (RowWin i p)  = "  player " ++ show p ++ " wins on row " ++ show i
  show (ColWin j p)  = "  player " ++ show p ++ " wins on column " ++ show j
  show (DiagWin d p) = "  player " ++ show p ++ " wins on the " ++ show d

instance Show DiagDir where
  show Major = "major diagonal"
  show Minor = "minor diagonal"

-- Prints out the board in a very attractive way.
-- Precond: The sizes of the broad must agree!
instance Show Board where
  show (Board brd) = unlines $ intersperse (hline (m+1)) $ map showRow rows
    where
      rows          = splitEvery (m+1) $ elems brd
      showRow       = intersperse '|' . concatMap show
      hline n       = intersperse '+' $ replicate n '-'
      (_,(m,n))     = bounds brd

player :: RCDJudgement -> Player
player (RowWin _ p)  = p
player (ColWin _ p)  = p
player (DiagWin _ p) = p

other :: Player -> Player
other O = R
other R = O
