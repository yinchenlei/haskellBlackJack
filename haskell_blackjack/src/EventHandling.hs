module EventHandling
 (
  handleBetting,
  handleEndRound,
  handleEndGame,
  handleAIPlay,
  handleChoice
 ) where

import Game
import Deck
import Player
import Brick.Widgets.Dialog
import System.Random

----------------------------------------------------------------------------------------
-- Betting Round Stage Event Handling Functions
----------------------------------------------------------------------------------------

handleBetting :: Game -> Game
handleBetting (Game players pot deck roundBet _ stdGen) = Game newPlayers newPot deck roundBet EndRound stdGen
  where -- human always has maximum bet, so just AI decide after
    (aiPlayers, newPot) = aiBetLoop (filter isAI players) roundBet pot []
    newPlayers = filter isHuman players ++ aiPlayers

aiBetLoop :: [Player] -> Integer -> Integer -> [Player] -> ([Player], Integer)
aiBetLoop [] _ pot acc = (acc, pot)
aiBetLoop (h:t) roundBet pot acc = aiBetLoop t newRoundBet newPot (acc ++ [ai])
  where
    (ai, newPot, newRoundBet) = aiBet h roundBet pot

----------------------------------------------------------------------------------------
-- End Round Stage Event Handling Functions
----------------------------------------------------------------------------------------

-- Create list of indices corresponding to winning players
-- Assume blackack is worth 22 for simplicity in calculations
findWinners :: [Player] -> Integer -> [Integer] -> Integer -> [Integer]
findWinners [] _ l _ = l
findWinners ((Human Fold _ _ _):t) maxScore l i = findWinners t maxScore l (i+1)
findWinners ((AI Fold _ _ _):t) maxScore l i = findWinners t maxScore l (i+1)
findWinners ((Human Stand _ _ hand):t) maxScore l i
  | h > maxScore = f h [i] (i+1)
  | h == maxScore = f maxScore (i:l) (i+1)
  | otherwise = f maxScore l (i+1)
  where
    score = checkScore hand
    h = if score == 21 && isBlackjack hand then 22 else score
    f = findWinners t
findWinners ((AI Stand _ _ hand):t) maxScore l i
  | h > maxScore = f h [i] (i+1)
  | h == maxScore = f maxScore (i:l) (i+1)
  | otherwise = f maxScore l (i+1)
  where
    score = checkScore hand
    h = if score == 21 && isBlackjack hand then 22 else score
    f = findWinners t
findWinners _ _ l _ = l -- error case

-- Distribute fraction of pot to all winning players
distributeMoney :: [Player] -> [Integer] -> Integer -> Integer -> [Player]
distributeMoney [] _ _ _ = []
distributeMoney ((Human _ money _ _):t) winners amt i = h:distributeMoney t winners amt (i+1)
  where h = if i `elem` winners
              then (Human Playing (money+amt) 0 [])
              else (Human Playing money 0 [])

distributeMoney ((AI _ money _ _):t) winners amt i = h:distributeMoney t winners amt (i+1)
  where h = if i `elem` winners
              then (AI Playing (money+amt) 0 [])
              else (AI Playing money 0 [])

-- Check if Human is out of money (stop condition)
checkZeroMoney :: Player -> Bool
checkZeroMoney (Human _ m _ _) = m == 0

-- Check if game does not have enough cards to continue playing (stop condition)
-- Note: will fail if players use more than 4 cards each
checkOutOfCards :: [Player] -> Deck -> Bool
checkOutOfCards p d = length d < 4*length p

-- Perform end round house-keeping:
-- find winner of round, distribute cash, reset hands, reset pot, check enough cards and money
handleEndRound :: Game -> Game
handleEndRound (Game players pot deck roundBet stage stdGen) = Game newPlayers newPot deck 0 newStage stdGen
  where
    winners = findWinners players 0 [] 0
    splitAmt = if null winners then 0 else pot `div` fromIntegral (length winners)
    newPot = pot - (splitAmt * fromIntegral (length winners))
    newPlayers = distributeMoney players winners splitAmt 0
    newStage = if checkZeroMoney (head (filter isHuman players))
                   || checkOutOfCards players deck
                 then EndGame
                 else AIPlay

----------------------------------------------------------------------------------------
-- EndGame Stage Event Handling Functions
----------------------------------------------------------------------------------------

-- handleEndEndGame is a function that produces the indices in the set of players which correspond to
-- a winning player. It takes as arguments: players, the maximumScore seen, the list of winning indices,
-- and the current index;
handleEndGame :: [Player] -> Integer -> [Integer] -> Integer -> [Integer]
handleEndGame [] _ l _ = l
handleEndGame (Human _ money _ _:t) maxScore l i
  | money > maxScore = handleEndGame t money [i] (i+1)
  | money == maxScore = handleEndGame t maxScore (i:l) (i+1)
  | otherwise = handleEndGame t maxScore l (i+1)

handleEndGame (AI _ money _ _:t) maxScore l i
  | money > maxScore = handleEndGame t money [i] (i+1)
  | money == maxScore = handleEndGame t maxScore (i:l) (i+1)
  | otherwise = handleEndGame t maxScore l (i+1)

----------------------------------------------------------------------------------------
-- AI Play Stage Event Handling Functions
----------------------------------------------------------------------------------------

handleAIPlay :: Game -> Game
handleAIPlay (Game players pot deck roundBet AIPlay stdGen) = Game newPlayers newPot newDeck newRoundBet HumanPlay stdGen
  where
    (aiPlayers, newDeck, newRoundBet, newPot, newStdGen) = aiPlayLoop (filter isAI players) deck roundBet pot [] stdGen
    newPlayers = filter isHuman players ++ aiPlayers

aiPlayLoop :: [Player] -> Deck -> Integer -> Integer -> [Player] -> StdGen -> ([Player], Deck, Integer, Integer, StdGen)
aiPlayLoop [] deck roundBet pot acc stdGen = (acc, deck, roundBet, pot, stdGen)
aiPlayLoop (ai:t) deck roundBet pot acc stdGen = aiPlayLoop t newDeck newRoundBet newPot (acc ++ [newAI]) stdGen
  where (newAI, newDeck, newRoundBet, newPot, newStdGen) = aiPlay ai deck roundBet pot stdGen

----------------------------------------------------------------------------------------
-- Human Play Stage Event Handling Functions
-- These functions all handle actions taken by the human player
----------------------------------------------------------------------------------------

-- Top level choice handling function
-- Switches based on dialog input (HitC, FoldC, etc)
-- Threads through the Internal State (Game, Dialog)
handleChoice :: InternalState -> InternalState
handleChoice (Game players pot deck roundBet HumanPlay stdGen, dialog) =
   case choice of
     Just HitC -> (handleHit game, dialog)
     Just FoldC -> (handleStand game, dialog)
     Just StandC -> (handleStand game, dialog)
     Just BetC -> (handleBet game, dialog)

     -- Reset just resets the Game Data instead of threading it through
     Just ResetC -> (createInitialGame (fromIntegral (length players) - 1) stdGen, dialog)
     Nothing -> internalState -- error shouldn't get here
  where
    game = Game players pot deck roundBet HumanPlay stdGen
    internalState = (game, dialog)
    choice = dialogSelection dialog

-- If no other event matches our patterns, just thread through the current internal state
handleChoice internalState = internalState

-- HandleStand handles the StandC choice, by setting the Human players status to Stand (inside stand function)
-- It then updates the Game stage to BettingRound (as Standing ends the HumanPlay Stage)
-- It threads through the Game state
handleStand :: Game -> Game
handleStand (Game players pot deck roundBet stage stdGen) = Game newPlayers newPot deck bet newStage stdGen
   where
       (Human status money bet hand, newPot) = stand (head (filter isHuman players)) roundBet pot
       newPlayers = Human status money bet hand:filter isAI players
       newStage = BettingRound

-- HandleFold handles the FoldC choice, by setting the Human players status to Fold
-- It then updates the Game stage to BettingRound (as Standing ends the HumanPlay Stage)
-- It threads through the Game state
handleFold :: Game -> Game
handleFold (Game players pot deck roundBet stage stdGen) = Game newPlayers pot deck roundBet newStage stdGen
   where
       (Human status money bet hand) = head (filter isHuman players)
       newPlayers = Human Fold money bet hand:filter isAI players
       newStage = BettingRound

-- HandleHit handles the HitC choice. It first draws a card from the deck.
-- Depending on whether or not this card caused the human player to bust, the stage is set to HumanPlay
-- BettingRound
handleHit :: Game -> Game
handleHit (Game players pot deck roundBet stage stdGen) = Game newPlayers pot newDeck roundBet newStage newStdGen
  where
    humanPlayer = head (filter isHuman players)
    (Human status money bet hand, newDeck, newStdGen) = draw humanPlayer deck stdGen
    newStage = if status == Fold then BettingRound else HumanPlay
    newPlayers = Human status money bet hand:filter isAI players

-- HandleBet handles the BetC Choice. It updates the Game's pot and deducts amounts from the players money.
handleBet :: Game -> Game
handleBet (Game players pot deck roundBet stage stdGen) = Game newPlayers newPot deck (max playerBet roundBet) stage stdGen
  where
    humanPlayer = head (filter isHuman players)
    (Human _ oldMoney _ _) = humanPlayer
    (Human status money playerBet hand, newPot) = bet humanPlayer pot
    newPlayers = Human status money playerBet hand:filter isAI players
