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
  initialise = initialiseCLI,
  onDisplayGameState = print,
  onDisplayBoardState = print,
  onPlayersTurn = onPlayersTurnCLI,
  onGameDraw = onGameDrawCLI,
  onGameWin  = onGameWinCLI,
  onGameLose = onGameLoseCLI
}

initialiseCLI :: GameState -> IO ()
initialiseCLI (GameState {human=plyr,computer=comp}) = do
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "!            Hok's Noughts and Arrs               !\n"
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "  For Roo.\n\n"
  putStr $ "You, the player, plays '" ++ show plyr
           ++ "', whereas the computer plays '" ++ show comp ++ "'.\n"
  putStr "So, we start the game!\n\n"

-- Produces a valid move from some valid game state.
onPlayersTurnCLI :: Player -> GameState -> IO Move
onPlayersTurnCLI plyr (GameState {boardState=bs}) = do
  putStr $ "(" ++ show plyr ++ "'s turn) Where will you go next?\n"
  coord <- msum (repeat $ getUserCoordCLI bs)
  putStr "\n\n"
  return $ ((turn bs),coord)

onGameDrawCLI :: IO ()
onGameDrawCLI = putStr drewMsg
  where
    drewMsg = "...................................................\n \
              \---------------------- DRAW -----------------------\n"

onGameWinCLI :: Player -> IO ()
onGameWinCLI _ = putStr congratulateMsg
  where
    congratulateMsg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\
                      \Congratulations, you've won against the mighty computer!!\n\
                      \You're so smart!\n\
                      \!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"

onGameLoseCLI :: Player -> IO ()
onGameLoseCLI _ = putStr insultMsg
  where
    insultMsg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n \
                \POOR YOU! You've LOST! Oh deary deary me.             \
                \On such a simple game as well!\n\
                \!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"


-- Does the hard work of extracting a game state from the user.
getUserCoordCLI :: BoardState -> IO (Int,Int)
getUserCoordCLI bs@(BoardState {turn=thePlayer}) = do
  showPrompt "> "
  inLine <- getLine      -- retrieve the line -- other 
  case parse coordinate "Failed to parse coordinate" inLine of
    Left _  -> parseErrorMsg >> mzero
    Right c -> if (thePlayer,c) `isValidIn` bs
               then return c
               else validationErrorMsg c >> mzero
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

    validationErrorMsg c = do
      putStr $ show c ++ " is not valid (either occupied or out of bounds). Try again.\n"


-- Indent the string by some number of spaces, respecting where newlines are.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
