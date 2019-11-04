module Main where

import Deck
import Brick
import Player
import Draw
import Game
import EventHandling

import Brick.BChan
import Brick.Types
import Brick.Widgets.Dialog

import Graphics.Vty
import Control.Concurrent
import Control.Monad
import System.Random

----------------------------------------------------------------------------------------
-- Top Level Event Handlers
----------------------------------------------------------------------------------------

appEventHandler :: InternalState -> BrickEvent () CustomEvent -> EventM n (Next InternalState)

-- Exit Events
appEventHandler internalState (VtyEvent (EvKey KEsc [])) = halt internalState

---- Handle event when cursor is selected
appEventHandler internalState (VtyEvent (EvKey KEnter [])) =
  do
    let newState = handleChoice internalState
    continue newState

-- Handle moving the cursor around the dialog
appEventHandler internalState (VtyEvent event) = do
  let (game, dialog) = internalState
  nextDialog <- handleDialogEvent event dialog
  let newState = (game, nextDialog)
  continue newState

-- otherwise we handle events based on game stage
appEventHandler internalState _ =
  case stage of
    AIPlay -> continue (handleAIPlay game, dialog)
    BettingRound -> continue (handleBetting game, dialog)
    EndRound -> continue (handleEndRound game, dialog)
    EndGame -> halt internalState
    _ -> continue internalState
  where
    (game, dialog) = internalState
    (Game _ _ _ _ stage _) = game

app :: App InternalState CustomEvent ()
app = App {
  appDraw = drawUI,
  appChooseCursor = showFirstCursor,
  appHandleEvent = appEventHandler,
  appStartEvent = return,
  appAttrMap = const attributeMap
}

-- Main function for Haskell Blackjack
main :: IO ()
main = do

  -- User input to set number of AI
  putStrLn "How many AI to play against"
  input <- getLine
  stdGen <- getStdGen
  
  -- We cap number of AI at 3 and require input to be an integer
  let numberOfAI = min 3 $ read input :: Integer
  let initialGame = createInitialGame numberOfAI stdGen
  let initialState = (initialGame, initialDialog)

  -- Create FIFO to feed BackgroundTask
  -- Used for Brick Library's event handler framework
  channel <- newBChan 10
  forkIO $ forever $
    writeBChan channel BackgroundTask

  -- Initialization for brick game loop
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty

  -- Execute HaskellBlackjack
  (finalGameState, dialog) <- customMain initialVty buildVty (Just channel) app initialState
  let (Game players _ _ _ _ _) = finalGameState

  -- Determine the winners of the game and print them to console.
  let winningPlayerNumbers = handleEndGame players 0 [] 0
  print $ endGameOutput winningPlayerNumbers
