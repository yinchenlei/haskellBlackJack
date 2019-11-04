module Draw
 (
  drawUI,
  attributeMap
 )
 where

import Deck
import Brick
import Game
import Player
import Brick.Widgets.Core
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Types
import Brick.Widgets.Dialog
import Graphics.Vty

-- Top level function to render all the game components to screen
-- <=> defined in Brick API library as binary operator (A <=> B) such that
-- A is above B.
-- hBox just places a list of components horizontally accross the screen
drawUI :: InternalState -> [Widget ()]
drawUI internalState =
  [
    hBox (drawAIs computerPlayers 1 15)
    <=> drawPot (fromIntegral pot)
    <=> drawHuman humanPlayer roundBet 30
    <=> drawDialog dialog
    <=> str (show stage)
  ]
  where
    (Game players pot deck roundBet stage stdGen, dialog) = internalState
    computerPlayers = filter isAI players
    humanPlayer = head (filter isHuman players)

-- Draw function to draw players to screen
-- Has arguments (width, height), padding, text body and title
drawPlayer :: (Int, Int) -> Int -> String -> String -> Widget ()
drawPlayer (width, height) padding playerText label =
  padLeft (Pad padding)
  $ borderWithLabel (str label)
  $ drawRectangleWithText width height playerText

-- Draw function to draw AI players to screen
-- Has arguments players (list of AI players),
-- aiNumber (a counter to support ai Title, Eg AI-1, AI-2...) and padding
-- returns the list of widgets to be drawn
drawAIs :: [Player] -> Int -> Int -> [Widget ()]
drawAIs (h:t) aiNumber padding = drawAI h aiNumber padding : drawAIs t (aiNumber + 1) 15
drawAIs [] _ padding = []

-- Draw function to draw a single AI player to screen
-- Takes a player type (AI), the aiNumber, padding, and returns a widget to be drawn
drawAI :: Player -> Int -> Int -> Widget ()
drawAI player aiNumber padding =
  drawPlayer (20, 20) padding computerText $ "AI-" ++ show aiNumber
  where
    (AI status money bet hand) = player
    numCards = length hand
    computerText = "$" ++ show money ++ "\nStatus: " ++ show status ++ "\nBet: " ++ show bet ++ "\nCards: " ++ show numCards

-- Draw function to draw Human players to screen
-- Takes a human player, the current bet amount, padding and returns the widget to be drawn.
drawHuman :: Player -> Integer -> Int -> Widget ()
drawHuman player currBet padding =
  drawPlayer (80, 40) padding humanText "You"
  where
     (Human status money bet hand) = player
     humanText = "Current Bet Required: $" ++ show currBet ++"\n\nMoney: $" ++ show money ++ "\nStatus: " ++ show status ++ "\nBet: " ++ show bet ++ "\nHand: " ++ outputHand hand

-- Draw function to display the current pot (sum of current bets) to screen
-- Takes the value of the pot and returns the widget to be drawn.
drawPot :: Int -> Widget ()
drawPot pot =
  padLeft (Pad 55)
  $ borderWithLabel (str "Pot")
  $ drawRectangleWithText 30 20 (show pot)

-- Draw function to display the dialog to screen
-- Takes the current dialog and returns a widget to be drawn
-- Relies on Brick library renderDialog function
drawDialog :: Dialog Choice -> Widget ()
drawDialog dialog =
  renderDialog dialog
  $ center
  $ padAll 5
  $ str ""

-- Lower level draw function to draw rectangles to screen
-- Takes width, height and text to display. Returns the widget to be drawn.
drawRectangleWithText :: Int -> Int -> String -> Widget()
drawRectangleWithText width height text =
  hLimit width (vLimitPercent height (center (str text)))

-- dummy attribute map to satisfy App Data in main. Used in the background by Brick API
attributeMap :: AttrMap
attributeMap = attrMap defAttr
  [ (borderAttr, white `on` black),
    (buttonSelectedAttr, bg yellow)
  ]
