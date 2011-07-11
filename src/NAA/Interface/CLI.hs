module NAA.Interface.CLI (cliInterface) where

import NAA.Interface
import NAA.Data
import NAA.State
import NAA.Logic
import System.IO
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Data.Array.Diff

cliInterface = UserInterface {
  onInitialise  = initialiseCLI,
  onGameStart   = onGameStartCLI,
  onRetrieveMove = onRetrieveMoveCLI,
  onInvalidMove = onInvalidMoveCLI,
  onPlayerMove  = onPlayerMoveCLI,
  onGameEnd     = onGameEndCLI,
  onTerminate   = return ()
}

initialiseCLI :: IO ()
initialiseCLI = do
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "!            Hok's Noughts and Arrs               !\n"
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "  For Roo.\n\n"

onGameStartCLI :: GameState -> IO ()
onGameStartCLI (GameState {human=plyr,computer=comp}) = do
  putStr $ "You, the player, plays '" ++ show plyr
           ++ "', whereas the computer plays '" ++ show comp ++ "'.\n"
  putStr "So, we start the game!\n\n"

-- Produces a valid move from some valid game state.
onRetrieveMoveCLI :: Player -> GameState -> IO Move
onRetrieveMoveCLI plyr (GameState {boardState=bs}) = do
  print $ board bs
  putStr "\n"
  putStr $ "(" ++ show plyr ++ "'s turn) so where will you go next?\n"
  coord <- getPlayerCoordCLI
  putStr "\n\n"
  return $ ((turn bs),coord)

onInvalidMoveCLI :: Move -> IO ()
onInvalidMoveCLI (p,c) = do
  putStr $ show c ++ " is not valid (either occupied or out of bounds). Try again.\n"

-- We don't actually need to print the board again, since it's done
-- every time the player is prompted anyway.
onPlayerMoveCLI :: Move -> GameState -> IO ()
onPlayerMoveCLI _ (GameState {boardState=BoardState {board=brd}}) = return ()

onGameEndCLI :: GameState -> BoardJudgement -> IO ()
onGameEndCLI (GameState {human=p,boardState=bs}) result = do
  print $ board bs
  case result of
    Draw      -> putStr drewMsg
    Win p'    -> if p == p'
                 then putStr congratulateMsg
                 else putStr insultMsg
    Invalid i -> error . show $ Invalid i
  where
    drewMsg = "...................................................\n \
              \---------------------- DRAW -----------------------\n"

    congratulateMsg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\
                      \Congratulations, you've won against the mighty computer!!\n\
                      \You're so smart!\n\
                      \!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"

    insultMsg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n \
                \POOR YOU! You've LOST! Oh deary deary me.\
                \On such a simple game as well!\n\
                \!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"

-- Does the hard work of extracting a game state from the user.
getPlayerCoordCLI :: IO (Int,Int)
getPlayerCoordCLI = do
  showPrompt "> "
  inLine <- getLine      -- retrieve the line -- other 
  case parse coordinate "Failed to parse coordinate" inLine of
    Left _  -> parseErrorMsg >> mzero
    Right c -> return c
  where
    showPrompt prompt = putStr prompt >> hFlush stdout -- stdout buffers until \n.

    coordinate :: Parser (Int,Int)
    coordinate = do { spaces;
                      char '('; i <- int; char ','; j <- int; char ')';
                      spaces; return (i,j)
                    }
    
    int :: Parser Int
    int = do
      n <- many (oneOf "0123456789")
      return $ read n

    parseErrorMsg = do
      putStr "\nA coordinate must be in the form (x,y). Try again.\n"


-- Indent the string by some number of spaces, respecting where newlines are.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
