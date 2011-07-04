{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import NAA.AI
import NAA.Loop
import NAA.Logic
import NAA.Data hiding (player)
import NAA.Interface (initialise)
import NAA.Interface.CLI
import Control.Monad.Trans
import Control.Monad.State.Lazy

-- *Very* dubious initialisation function
main :: IO ()
main = do

  let player = human defaultGameState
  let comp   = computer defaultGameState

  -- Set the user interface here
  let iface = cliInterface
  let gs = defaultGameState

  _ <- execStateT (liftIO (initialise iface gs) >> runNoughtsAndArrs iface) gs
  return ()
  where
    default3x3Board = createBoard3x3 $ replicate 9 Empty
    defaultGameState = GameState {
      theBoard = default3x3Board,
      turn = R,
      wins = 0,
      losses = 0,
      human = R,
      computer = O
    }
