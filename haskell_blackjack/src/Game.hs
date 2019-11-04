module Game
  (
    Player(..),
    Status(..),
    Stage(..),
    Game(..),
    Choice(..),
    InternalState,
    CustomEvent(..),
    createInitialGame,
    initialDialog,
    endGameOutput
  )
where

import Deck
import Brick.Widgets.Dialog
import System.Random

----------------------------------------------------------------------------------------
-- Game Data Structures and Types
----------------------------------------------------------------------------------------

-- Data representing a player in this game
-- Defined by parameters status, currentMoney (Integer), currentBet (Integer) and cards (hand)
data Player = Human Status Integer Integer [Card]
            | AI Status Integer Integer [Card]
            deriving (Show, Eq)

-- The different status that a player may be in
data Status = Stand | Fold | Playing
  deriving (Show, Eq)

-- The stages of the game we can be in
data Stage = AIPlay | HumanPlay | BettingRound | EndRound | EndGame
  deriving (Show, Eq)

-- Data representing a game (state of the game)
-- Defined by: Players, current pot, remaining deck, Maximum bet seen, current Stage, and a Random Generator
data Game = Game [Player] Integer Deck Integer Stage StdGen
  deriving (Show)

-- Choices a user has access to in the Dialog
data Choice = HitC | StandC | FoldC | BetC | ResetC
  deriving (Show)

-- Overall internal state that gets thread through interactions with the Game loop (Brick API)
type InternalState = (Game, Dialog Choice)

-- Custom Event type so that we can process more than just keyboard actions
-- This is meant to trigger background processing tasks such as AIPlaying and betting actions
data CustomEvent = BackgroundTask deriving Show

----------------------------------------------------------------------------------------
-- Initial State functions
----------------------------------------------------------------------------------------

createInitialGame :: Integer -> StdGen -> Game
createInitialGame numberOfAi stdGen = Game players 0 createFullDeck 0 AIPlay stdGen
  where players = Human Playing 500 0 [] : [AI Stand 500 0 [] | i <- [1..numberOfAi]]

initialDialog = dialog (Just "Controls") (Just (0, choices)) 70
  where choices = [("Hit", HitC), ("Bet $25", BetC), ("Stand/Match Bet", StandC), ("Fold", FoldC), ("Reset", ResetC)]

----------------------------------------------------------------------------------------
-- Final State functions
----------------------------------------------------------------------------------------

endGameOutput indices = "Winners: " ++ foldr (\index output ->
  if index == 0 then output ++ "Human Player " else output ++ "AI Player-" ++ show index ++ " ") "" indices
