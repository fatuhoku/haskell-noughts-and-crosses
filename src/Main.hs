{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import NAA.AI
import NAA.State
import NAA.Loop
import NAA.Logic
import NAA.Data hiding (player)
import NAA.Interface
import NAA.Interface.CLI
import Control.Monad.Trans
import Control.Monad.State.Lazy

main :: IO ()
main = do
  onInitialise iface
  execStateT (runNoughtsAndArrs iface) gs
  onTerminate iface
  where
    iface = cliInterface              -- Use the Command Line Interface
    gs = GameState {                  -- A default initial game state.
      boardState = blankBoardState R,
      wins = 0,
      losses = 0,
      human = R,
      computer = O
    }
