{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import NAA.AI
import NAA.Logic
import NAA.Data hiding (player)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Data.Array.Diff
import System.IO

type NoughtsAndArrs a = StateT GameState IO a

thePlayer = R
theComputer = O

-- At the beginning of the game, I start with a totally blank 3x3 board.
main :: IO ()
main = do
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "!            Hok's Noughts and Arrs               !\n"
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "  For Roo.\n\n"
  putStr $ "You, the player, plays '" ++ show thePlayer
           ++ "', whereas the computer plays " ++ show theComputer ++ ".\n"
  putStr "So, we start the game!\n\n"
  _ <- execStateT runNoughtsAndArrs $ defaultGameState
  return ()
  where
    defaultGameState = GameState default3x3Board R 0 0
  
default3x3Board = createBoard3x3 $ replicate 9 Empty

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
runNoughtsAndArrs :: NoughtsAndArrs ()
runNoughtsAndArrs = do

  -- Read state and show the game status
  gs  <- get
  gs' <- withGameState gs $ \gs -> do
    print gs
    let player = turn gs
    move <- performTurn player gs    -- obtain a move from the AI or player
    -- Put the new nought or cross into the state
    let newBoard = theBoard gs `apply` move
    let gs' = gs {theBoard=newBoard,turn=other player}
    return gs'
  put gs'

  case judge (theBoard gs') of
    Just result -> liftIO $ do
      print gs'
      putStr $ case result of
        Draw      -> drewMsg
        Win p     -> if p == thePlayer
                     then congratulateMsg thePlayer
                     else insultMsg thePlayer
        Invalid i -> error . show $ Invalid i
    Nothing -> runNoughtsAndArrs             -- neither won or lost. Continue.
  where
    withGameState gs mf = liftIO (mf gs)
    -- Gets a valid coordinate for the game state gs. 
    retrieveCoords :: GameState -> IO (Int,Int)
    retrieveCoords gs = do
      showPrompt "> "
      inLine <- getLine      -- retrieve the line
      case parse coordinate "Failed to parse coordinate" inLine of
        Left _  -> parseErrorMsg >> mzero
        Right c -> if c `isValidIn` gs
                   then return c
                   else validationErrorMsg c >> mzero

    isValidIn :: (Int,Int) -> GameState -> Bool
    isValidIn c@(i,j) (GameState {theBoard=Board brd}) =
      minM <= i && i <= maxM &&
      minN <= j && j <= maxN &&
      brd ! c == Empty
      where
        ((minM,minN),(maxM,maxN)) = bounds brd

    coordinate :: Parser (Int,Int)
    coordinate = do { spaces;
                      char '('; i <- int; char ','; j <- int; char ')';
                      spaces; return (i,j)
                    }

    performTurn p
      | p == thePlayer   = thePlayersTurn
      | p == theComputer = theComputersTurn
      | otherwise = error "Perform turn found an invalid player."
    
    -- this is where the AI can be plugged in
    theComputersTurn gs = unbeatableAI theComputer gs

    thePlayersTurn gs = do
      putStr "Where will you go next?\n"
      coord <- msum (repeat $ retrieveCoords gs)
      putStr "\n\n"
      return $ Move ((turn gs),coord)

    int :: Parser Int
    int = do
      n <- many (oneOf "0123456789")
      return $ read n

    congratulateMsg _ = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n \
                        \Congratulations, you've won against the mighty computer!!\n\
                        \You're so smart!\n"

    insultMsg _ = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n \
                  \POOR YOU! You've LOST! Oh deary deary me.             \
                  \On such a simple game as well!\n"

    drewMsg = "...................................................\n \
              \---------------------- DRAW -----------------------\n"

    parseErrorMsg = do
      putStr "\nA coordinate must be in the form (x,y). Try again.\n"

    validationErrorMsg c = do
      putStr $ show c ++ " is not valid (either occupied or out of bounds). Try again.\n"

    showPrompt prompt = putStr prompt >> hFlush stdout -- stdout buffers until \n.

-- Indent the string by some number of spaces, respecting where newlines are.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
