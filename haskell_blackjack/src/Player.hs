
module Player
  (
    isHuman,
    isAI,
    checkScore,
    draw,
    stand,
    bet,
    aiPlay,
    aiBet
  ) where

import Deck
import Game
import System.CPUTime
import System.Random

----------------------------------------------------------------------------------------
-- Helper functions for Players
----------------------------------------------------------------------------------------

--Take a player and return true if the player is human, false otherwise
isHuman :: Player -> Bool
isHuman (Human _ _ _ _) = True
isHuman _ = False

--Take a player and return true if the player is AI, false otherwise
isAI :: Player -> Bool
isAI (AI _ _ _ _) = True
isAI _ = False

-- takes a list of cards on hand, and returns the minimum combination value, since Ace could represent 1 or 11.
checkBust :: [Card] -> Integer
checkBust hand = minimum $ valueDeck hand

-- takes a list of cards on hand, and returns the maximum combination value on hand which is < 21, since Ace could represent  1 or 11.
checkMaximum :: [Card] -> Integer
checkMaximum hand = if null values then 0 else maximum values
  where values = filter (<= 21) (valueDeck hand)

checkScore :: [Card] -> Integer
checkScore hand = if null scores then 0 else maximum scores
  where scores = filter (<= 21) (valueDeck hand)

----------------------------------------------------------------------------------------
-- Player moves
----------------------------------------------------------------------------------------

 -- Draw randomly take a card from the Deck and give it to the player ("hit")
draw :: Player -> Deck -> StdGen -> (Player, Deck, StdGen)
draw (Human status money bet hand) deck stdGen = (Human newStatus money bet newHand, newDeck, newStdGen)
  where newHand = newCard:hand
        newCard = deck !! randomIndex
        (ys, zs) = splitAt randomIndex deck
        newDeck = ys ++ tail zs
        (randomIndex, newStdGen) = randomR (0, length deck - 1) stdGen
        newStatus = if checkBust newHand > 21 then Fold else Playing

-- randomly draw a card from the Deck and give it to the AI
draw (AI status money bet hand) deck stdGen = (AI newStatus money bet newHand, newDeck, newStdGen)
  where newHand = newCard:hand
        newCard = deck !! randomIndex
        (ys, zs) = splitAt randomIndex deck
        newDeck = ys ++ tail zs
        (randomIndex, newStdGen) = randomR (0, length deck - 1) stdGen
        newStatus = if checkBust newHand > 21 then Fold else Playing

-- stand function for human player
-- takes a human player, maxBet, pot
-- return an updated human player, and pot
stand :: Player -> Integer -> Integer -> (Player, Integer)
stand (Human _ m b h) roundBet pot
  | b >= roundBet = (Human Stand m b h, pot)
  | m-roundBet >= 0 = ((Human Stand (m-amt) (b+amt) h), pot+amt)
  | otherwise = (Human Fold m 0 [], pot)
    where
      amt = roundBet-b

--fold function for human player
--takes a human player, return an updated human player
fold :: Player -> Player
fold (Human _ m b h) = (Human Fold m 0 [])

--bet function for human player
--takes a human player and pot money, return an updated human player and pot money
bet :: Player -> Integer -> (Player, Integer)
bet (Human s money oldAmt h) pot = ((Human s (money-amt) (oldAmt+amt) h), pot+amt)
  where
    amt = if money - 25 < 0 then money else 25 -- all-in else $25 increments

----------------------------------------------------------------------------------------
-- AI Moves
----------------------------------------------------------------------------------------

-- AI moves at beginning of each round.
-- It Draws 2 or more cards, and the do the initial betting
-- takes an AI player, deck, maxBet, pot,and stdGen
-- returns an updated player, deck, maxBet, pot, and stdGen
aiPlay :: Player -> Deck -> Integer -> Integer -> StdGen -> (Player, Deck, Integer, Integer, StdGen)
aiPlay (AI status money bet hand) deck roundBet pot stdGen
  | checkBust hand > 21 = ((AI Fold money bet hand), deck, roundBet, pot, stdGen)
  | ((checkMaximum hand) + (averageCardValueFromDeck deck)) > 21 =
    let (ai, newPot, newRoundBet) =  aiBet (AI status money bet hand) roundBet pot
      in (ai, deck, newRoundBet, newPot, stdGen)
  | otherwise = aiPlay newPlayer newDeck roundBet pot newStdGen
                  where (newPlayer, newDeck, newStdGen) = draw (AI status money bet hand) deck stdGen

-- Betting logic for AIPlay round turn and BettingRound turn
-- betting strategy for AI based on the hand, roundBet, and its currentBet
-- AI might choose to bet more, follow, or fold based on the circumstance
-- takes a AI player, maxBet and pot money returns the updated player, pot money and maxBet
aiBet:: Player -> Integer -> Integer -> (Player, Integer, Integer)
aiBet (AI Fold money bet hand) maxBet pot = ((AI Fold money bet hand), maxBet, pot)
aiBet (AI _ money bet hand) maxBet pot
 | checkMaximum hand <= 16 = ((AI Fold money bet hand), pot, maxBet)
 | checkMaximum hand <= 18 && (maxBet == 0 && money <=50)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand <= 18 && (((maxBet-bet) > money) && money <=50)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand == 17 && (maxBet == 0 && (length hand) == 2)  = ((AI Stand (money-50) (bet+50) hand), pot+50 , 50)
 | checkMaximum hand == 17 && (maxBet == 0 && (length hand) >= 2)  = ((AI Stand (money-25) (bet+25) hand), pot+25 , 25)
 | checkMaximum hand == 17 && ((maxBet-bet) == 25 && bet <= 50) = ((AI Stand (money-25) (bet+25) hand), pot+25 , maxBet)
 | checkMaximum hand == 17 = ((AI Fold money bet hand), pot, maxBet)
 | checkMaximum hand == 18 && maxBet == 0  = ((AI Stand (money-50) (bet+50) hand), pot+50 , 50)
 | checkMaximum hand == 18 && ((maxBet-bet) <= 50 && bet <= 50) = ((AI Stand (money-(maxBet-bet)) maxBet hand), pot+(maxBet-bet), maxBet)
 | checkMaximum hand == 18 = ((AI Fold money bet hand), pot, maxBet)
 | checkMaximum hand == 19 && (maxBet == 0 && money <=75)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand == 19 &&( ((maxBet-bet) > money) && money <=75)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand == 19 && (maxBet == 0 && (length hand) == 2)  = ((AI Stand (money-75) (bet+75) hand), pot+75 , 75)
 | checkMaximum hand == 19 && (maxBet == 0 && (length hand) >= 2)  = ((AI Stand (money-50) (bet+50) hand), pot+50 , 50)
 | checkMaximum hand == 19 && ((maxBet-bet) == 50 && bet <= 100) =((AI Stand (money-50) (bet+50) hand), pot+50 , maxBet)
 | checkMaximum hand == 19 = ((AI Fold money bet hand), pot, maxBet)
 | checkMaximum hand <= 21 && (maxBet == 0 && money <=100)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand <= 21 && ((maxBet-bet) > money)= ((AI Stand 0 (bet+money) hand), pot+money , maximum [bet+money, maxBet])
 | checkMaximum hand <= 21 && (maxBet > bet) = ((AI Stand (money-(maxBet-bet)) maxBet hand), pot+(maxBet-bet), maxBet)
 | checkMaximum hand == 20 && (maxBet == 0 && (length hand) == 2)  = ((AI Stand (money-100) (bet+100) hand), pot+100 , 100)
 | checkMaximum hand == 20 && (maxBet == 0 && (length hand) >= 2)  = ((AI Stand (money-50) (bet+50) hand), pot+50 , 50)
 | checkMaximum hand == 21 && (maxBet == 0 && (length hand) == 2)  = ((AI Stand (money-100) (bet+100) hand), pot+100 , 100)
 | checkMaximum hand == 21 && (maxBet == 0 && (length hand) >= 2)  = ((AI Stand (money-50) (bet+50) hand), pot+50 , 50)
 | otherwise = ((AI Fold money bet hand), pot, maxBet)
