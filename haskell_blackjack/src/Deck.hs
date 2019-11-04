module Deck
    (
      Deck,
      Suit,
      CardValue,
      Card,
      emptyDeck,
      outputHand,
      isBlackjack,
      createFullDeck,
      valueDeck,
      averageCardValueFromDeck
    ) where

----------------------------------------------------------------------------------------
-- Deck Data Structures and Types
----------------------------------------------------------------------------------------

data Suit = Clubs
          | Hearts
          | Diamonds
          | Spades
      deriving (Show, Enum, Eq)

data CardValue = Ace
                | Two
                | Three
                | Four
                | Five
                | Six
                | Seven
                | Eight
                | Nine
                | Ten
                | Jack
                | Queen
                | King
      deriving (Show, Eq)

instance Enum CardValue where
  fromEnum Ace      = 1
  fromEnum Two      = 2
  fromEnum Three    = 3
  fromEnum Four     = 4
  fromEnum Five     = 5
  fromEnum Six      = 6
  fromEnum Seven    = 7
  fromEnum Eight    = 8
  fromEnum Nine     = 9
  fromEnum Ten      = 10
  fromEnum Jack     = 10
  fromEnum Queen    = 10
  fromEnum King     = 10

newtype Card = Card (CardValue, Suit)
  deriving (Show)

instance Eq Card where
  (Card (val1, suit1)) == (Card (val2, suit2)) = val1 == val2 && suit1 == suit2

type Deck = [Card]

----------------------------------------------------------------------------------------
-- Output helper functions for Deck and Card types
----------------------------------------------------------------------------------------

-- Convert deck to String
outputHand :: Deck -> String
outputHand h = "[ " ++ foldr outputHelper [] h ++ "]"

outputHelper :: Card -> String -> String
outputHelper (Card (val, suit)) acc = getSuitSym suit ++ getCardValueSym val ++ " " ++ acc

-- Return Suit as a symbol String
getSuitSym :: Suit -> String
getSuitSym suit =
  case suit of
   Spades -> "♠"
   Clubs -> "♣"
   Hearts -> "♥"
   Diamonds -> "♦"

-- Return CardValue as a String
getCardValueSym :: CardValue -> String
getCardValueSym val =
  case val of
   Ace      -> "A"
   Two      -> "2"
   Three    -> "3"
   Four     -> "4"
   Five     -> "5"
   Six      -> "6"
   Seven    -> "7"
   Eight    -> "8"
   Nine     -> "9"
   Ten      -> "10"
   Jack     -> "J"
   Queen    -> "Q"
   King     -> "K"

----------------------------------------------------------------------------------------
-- Deck and Card logic functions
----------------------------------------------------------------------------------------

emptyDeck :: Deck
emptyDeck = []

valueList = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
suitList = [Clubs, Hearts, Diamonds, Spades]

-- creates a full deck of 52 cards from values 1-10,J,Q,K,A and the four suits
createFullDeck :: Deck
createFullDeck = spades ++ clubs ++ diamonds ++ hearts
    where spades = map (\cardVal -> Card (cardVal, Spades)) valueList
          clubs = map (\cardVal -> Card (cardVal, Clubs)) valueList
          diamonds = map (\cardVal -> Card (cardVal, Diamonds)) valueList
          hearts = map (\cardVal -> Card (cardVal, Hearts)) valueList


-- valueDeck takes a Deck type and creates a list of ints which are all the possible values of this deck
-- there are more than one possible value since Ace can represent 1 or 11.
valueDeck :: Integral i => Deck -> [i]
valueDeck [] = [0]
valueDeck deck = valueDeckHelper cardValues 0
  where cardValues = map (\(Card (val, suit)) -> val) deck

valueDeckHelper :: Integral i => [CardValue] -> i -> [i]
valueDeckHelper [] sum = [sum]
valueDeckHelper (h:t) sum
  | h == Ace = valueDeckHelper t (sum + 1) ++ valueDeckHelper t (sum + 11)
  | otherwise = valueDeckHelper t (sum + fromIntegral (fromEnum h))

-- Determines if the current Hand (Deck) is a Blackjack (Ace and Face Card)
isBlackjack :: Deck -> Bool
isBlackjack (Card (a,_):Card (b,_):[]) =
   (a `elem` [Ace]) && (b `elem` [Ten, Jack, Queen, King])
   || (b `elem` [Ace] && a `elem` [Ten, Jack, Queen, King])
isBlackjack _ = False

-- Used by AI to determine the average card that will be drawn next
averageCardValueFromDeck:: Deck -> Integer
averageCardValueFromDeck [] = 0
averageCardValueFromDeck deck = div (valueDeckAI deck) (fromIntegral(length deck))

-- valueDeckAI takes a deck and returns the the total value of the deck, Ace represent 1 in this case
valueDeckAI:: Deck -> Integer
valueDeckAI [] = 0
valueDeckAI deck = valueDeckAIHelper cardValues 0
  where cardValues = map (\(Card (val, suit)) -> val) deck

valueDeckAIHelper :: [CardValue] -> Integer -> Integer
valueDeckAIHelper [] sum = sum
valueDeckAIHelper (h:t) sum = valueDeckAIHelper t (sum + fromIntegral (fromEnum h))
