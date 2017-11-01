{- 
  File      :   Card.hs, Deck.hs, GameLogic.hs, Main.hs, Player.hs 
  Copyright :   (c) Gianni Chen, 11/01/17
  Notes		:	I worked together with Andrew Mao for the first part of the assignment, so certain parts may look similar
  Contents  :   Passed Halloween working an UNO AI, yay!
-}

AI Behavior

- There are 2 computers in this game, COM1 and COM2

- COM1 runs dumbPickCard, which just picks the first card in the hand that can be actually played

- COM2 runs smartPickCard instead, which follows the following algorithm:
	1. If any player has less than 3 cards, the computer will try to discard the highest cost card in hand.
	2. Then, if the computer's hand has more than 5 cards, it will also play the highest cost card in hand, except for Wild.
	3. And finally, if none of the above conditions are satisfied, it will just run the holdWild strategy, which will attempt to play a legal card in such a way that the color matches the most popular color on hand. However, it will try to hold onto the Wild Card.

- Strategy number 1 is self explanatory as the computer attempts to dump all the high cost cards in hand in order to give the winning player the least amount of points in case of an UNO

- On the other hand, strategy number 2 will also attempt the same strategy as strategy 1 with the exception that the Wild card will be held in hand as that can lead to a winning condition. This is because there is likely a big amount of points in the computer's hand, so it makes sense to get rid of high cost cards, however, this is not as pressing as situation 1 since no one is on the verge of winning

- And finally, for strategy number 3, the computer will attempt the same strategy as strategy 2, but instead of playing the highest cost card, it will play the most popular color. This is because there are a reasonable number of cards in hand, and playing the most popular color (such as a Skip Red on a Skip Blue) will drive the computer closer to the winning condition