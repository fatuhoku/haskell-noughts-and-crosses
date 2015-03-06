{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import Control.Monad.Trans
import Control.Monad.State.Lazy

import NAA.AI
import NAA.State
import NAA.Loop
import NAA.Logic
import NAA.Data hiding (player)
import NAA.Interface
import NAA.Interface.GL


-- We abstract the looping behaviour of the game from each of the components
-- that need updating.
--
-- The stages of the game can be seen as a set of filters that enrich and modify
-- different sections of the game state.
--
-- Perhaps later, combinators can be written to control the relative frequency
-- of how often each of these filters are executed as well.
--
-- Step - non-blocking 
-- --------------------------------------------
--
-- State GameInput  (RawInput filters update this numerous times before the
-- whole game input object is consumed)
--
-- (AI)
--   unbeatableAi :: Move -> 
--
--   something that inspects the current game state, and produces an AI decision
--   on a move.
--
-- Game
--   allow the game logic to update game state with updated inputs.
--   The new state is remembered and fed back into the next loop iteration.
--              We require that this does not block.
--
-- AppState
--   this describes the current 'mode' of the application: whether
--   there is a game in progress, whether there is 
--
-- Render (GameRender, ApplicationRender)
--   renders the current state of the application presents the updated state of
--   the game on screen! What could be better!
--
--
--   interpretInputs :: RawInput -> GameState -> Move
--   
--
--
-- The problem with this approach is that any of the state is accessible to
-- anybody at any time. This is undesirable. We wish that part of the state be
-- available to part of the step.
--
--
-- IDEAL
-- --------------------------------
-- The APPLICATION serves as a running 'container' for SERVICES that run within
-- the main loop.
--
-- <<< EXTERNAL PROGRAM INVOCATION >>>
-- At application start, the Application lifetime begins.
-- At application end, the Application lifetime ceases (can be controlled within loop)
--
-- We want to express first-class deltas using fclabels.
-- e.lg. 
--
-- If we run GLFW, the application is inherently less event-driven than the
-- command line interface.
--
-- Input filtering (Input processing)
-- -----------------------------------
--
-- RawInput
--   Input events from GLFW (or at least, those that we care about) are recorded
--   in IOVars and MVars. This stage occurs automatically occurs with a call to
--   swapBuffers. TODO disable the poll for fine-grain control.
--   We assume that a large number of events are produced, and filters must be
--   able to incrementally deal with small-step updates of raw input:
--   i.e. in one swapBuffer call we assume that 5 mouse position events can have
--   happened.
--
--   |
--   v
--
-- Monadic event handler:
-- We complete a monadic computation for every kind of game input. Because the
-- state kept could be very different in nature, a categorical grouping of
-- IORefs and operation in the IO monad would be necessary.
-- These are in effect 'chained' event handlers.
--
-- type StateFilter a = (NState a) => StateT RawInput IO ((a :-> a) -> (a :-> a))
-- type StateUpdateFunction a = (NState a) => IORef (a :-> a)
--
-- class NState
-- instance NState GameState
-- instance NState OverlayState
-- instance NState AppState
--
-- gsUpdateFunc :: StateUpdateFunction GameState
-- olUpdateFunc :: StateUpdateFunction OverlayState
-- appUpdateFunc :: StateUpdateFunction AppState
--
-- applyStateFunc :: (NState a) => StateUpdateFunction a -> NState a -> NState a
--
-- We build from raw input, in input filtering, first class state update functions
-- by composition.
--
-- The state updates can then be naturally applied onto their appropriate state.
--
-- Per-event type filter (Maybe keypress KeyboardFilter, [InputFilter]
--   The raw inputs are inspected by a list of 'input filters'. These perform further
--   IO actions to set other bits of State living somewhere else. (lifting?)
--   Filters may consume input, in which case, they return an updated RawInput object
--   which nullifies the input they wish to consume (fclabels? generic nullification?)
--  
--   where, any RawInput can be consumed by
--   the This list may include Application, Overlay, Game, GameElement level
--
main :: IO ()
main = withGlfw $ withGlfwWindow "Noughts and Arrs" winSize $ do

  -- Mainloop states --

  -- Create the GlfwInterface functions, ensuring that they have references to
  -- the IORefs
  let iface = genGlfwInterface
  let vars = Vars loopCtrlRef sceneRef

  setupGlfwCallbacs vars
  mainLoop vars
  where
    winSize = Size 600 600
    gs = GameState {                  -- A default initial game state.
      boardState = blankBoardState R,
      wins = 0,
      losses = 0,
      human = R,
      computer = O
    }
