module Deck (
    Deck(..),
    newDeck, 
    draw
) where 

import Card 

newtype Deck = Deck [Card]
    deriving (Show, Eq)  

{- 
   This variable returns a deck with all 108 cards for Uno. 

   See the image as an example: https://en.wikipedia.org/wiki/Uno_(card_game)#/media/File:UNO_cards_deck.svg

   You do not need to shuffle this deck. I will do that in my GUI code. 

-}

wildList = [Wild, Wild, Wild, Wild, Draw4, Draw4, Draw4, Draw4]
colorList = [Red, Green, Blue, Yellow]
typeList = [Skip, Reverse, Draw2]

{- Creates a new deck by adding the specific list of cards -}
newDeck :: Deck 
newDeck = Deck $ (addNumbers . addNumbers . addSpecial . addSpecial . addZeros) wildList

{- Adds the special types of cards to the list: Skip, Reverse, Draw2 -}
addSpecial :: [Card] -> [Card]
addSpecial list = list ++ [(cardType color) | color <- colorList, cardType <- typeList]

{- Adds the numbered cards from 1 to 9 -}
addNumbers :: [Card] -> [Card]
addNumbers list = list ++ [Card {color=color, rank=rank} | color <- colorList, rank <- [1..9]]

{- Adds the zero cards, needs to be called only once -}
addZeros :: [Card] -> [Card]
addZeros list = list ++ [Card {color=color, rank=0} | color <- colorList] 

{- 
   This function takes the top most card from the deck and returns a new deck with that card removed 
   and the card
 -}

draw :: Deck -> (Deck, Card)
draw (Deck (card:rest)) = (Deck rest, card)