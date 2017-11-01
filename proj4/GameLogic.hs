module GameLogic (
    GameState(..),
    GameStatus(..),
    turnCom, 
    turnHuman,
    isHandOrGameOver, 
)where 

import Deck 
import Card 
import Player 

{- A game status type that is used to signal to the GUI that current state of the game -}
data GameStatus = Active | GameOver | RoundOver deriving (Show, Eq)  


{- A game state type keeps track of the deck and states of the players in the game  -}
data GameState  = GameState { deck :: Deck, players :: [PlayerState]} deriving (Show, Eq)  

{- 
   I know this description looks like a lot but I'm trying to be as detailed as possible. 

   The "turnCom" function is called by the GUI for a computer's turn. Here's a description of each 
   parameter for this function: 

   ===== 
   gState :: GameState ==> The current state of the game. You will update this state at some point in this function
   and return a new GameState with those changes. 

   player :: Player ==> This parameter specifies which computer turn it is (i.e. COM1 or COM2)
   
   (card, color) :: (Card, Maybe Color) ==> This parameter represents the current up card. The reason why it's a tuple is that
   the previous player may have placed a "wild" special card down as the up card. This means the previous player must have indicated a color.  
   Thus, the second component is a Maybe value because it may have a Color specified if the card is a "wild" type of card or not. 

    randomNum :: Integer ==> This parameter represents a random number that is either: 1,2,3, or 4. You may want to use this to determine the 
                color for a wild card the computer may place down. 
    ====== 
    
    Based on a the rules of the game , the current up card, and the strategy of the computer, you will need to update the GameState and maybe
    return a card for this computer's turn. 

    "Updating the GameState" means that the computer may need to draw cards from the deck and place those cards in the computer's hand or remove 
    cards from the computer's hand. Thus, you are modifying the GameState. Remember you need to adhere to the rules of Uno. Thus, you need to look at the up card first and adhere to 
    the rules about the up card and then proceed with the strategy for the computer. 
    
    So why is this function returning a tuple as such: (GameState, Maybe (Card, Maybe Color)) ? 

    1st component: "GameState" ==> Represents the update game state after playing a computer's turn. 
    2nd component: Maybe (Card, Maybe Color) ==> Based on the rules, the computer may not place down a card if it does not have 
        a valid card (e.g., no cards in its hand matches the color or number of the up card). If the computer cannot put down a card then "Nothing" is returned. 
        Otherwise, the computer returns the card it wants to put down. If that card is a "Wild" type of card then it needs to specify the color for the next turn. 

        For example:  
          Just (Wild, Red) ===> This means the computer is placing down a wild card with the color being Red 
          Just (Card Red 3) ==> This means the computer is placing down a Red 3 card.  
          Nothing ==> This means the computer has no card to place down. 

-}

{- The computer first picks a card, and depending on the decision, will either play that card or draw a new card from the deck -}
turnCom :: GameState -> Player -> (Card, Maybe Color) -> Int -> (GameState, Maybe (Card, Maybe Color)) 
turnCom gState playerName upCard randomNum =
  if decision /= Nothing
    then checkForEmptyHand (newGState {players = removeCard newGState (getCardIndex decision cardList) playerName}, decision) playerName
    else (drawCard newGState upCard playerName randomNum)
  where
      newGState = checkSpecialCard gState playerName upCard
      cardList = getCardList newGState playerName
      decision = 
        if playerName == COM1
          then dumbPickCard upCard cardList randomNum 
          else smartPickCard newGState upCard cardList

{- Given the gamestate and the player, look for the cardlist -}
getCardList :: GameState -> Player -> [Card]
getCardList gState playerName = 
  let Hand cardList = hand (head (filter (\pState -> if (player pState) == playerName then True else False) (players gState))) 
  in cardList

{- Given a list of cards and the currently up card, will return a list of all legal cards -}
getAllLegalCards :: [Card] -> (Card, Maybe Color) -> [Card]
getAllLegalCards cardList upCard = 
  foldl (\legalCards aCard -> 
    if checkValidity aCard upCard 
      then legalCards ++ [aCard] 
      else legalCards) 
  [] cardList

{- CHECK FOR ROUND WIN CONDITION AND SCORING -}

{- Will first check for an empty hand, if empty, will tally up scores -}
checkForEmptyHand :: (GameState, Maybe (Card, Maybe Color)) -> Player -> (GameState, Maybe (Card, Maybe Color))
checkForEmptyHand (gState, a) playerName = 
  if cardList == []
    then (gState {players = changeScore pStateList playerName addScore}, a)
    else (gState, a)
  where
    cardList = getCardList gState playerName
    pStateList = players gState 
    addScore = tallyScores pStateList

{- Changes the score and returns the new pState with the updated player states -}
changeScore :: [PlayerState] -> Player -> Integer -> [PlayerState]
changeScore pStateList playerName addScore = 
  foldl (\list pState -> 
    if (player pState) == playerName 
      then list ++ [(pState {score = (score pState) + addScore})] 
      else list ++ [pState]) 
  [] pStateList

{- Sums the scores for all the players -}
tallyScores :: [PlayerState] -> Integer
tallyScores pStateList = foldl (\sum pState -> sum + scoreHand (hand pState)) 0 pStateList

{- Sums the score for all the cards in a player's hand -}
scoreHand :: Hand -> Integer
scoreHand (Hand cardList) = foldl (\sum card -> sum + (cardCost card)) 0 cardList

{- CHECK DRAW CARDS -}

{- Check if the player needs to draw cards, and then draw cards otherwise return same gamestate -}
checkSpecialCard :: GameState -> Player -> (Card, Maybe Color) -> GameState
checkSpecialCard gState playerName (Draw4, _) = drawCards gState playerName 4
checkSpecialCard gState playerName ((Draw2 _), _) = drawCards gState playerName 2
checkSpecialCard gState _ _ = gState

{- Draws a specific number of cards from the deck for a specific player and returns new gamestate -}
drawCards :: GameState -> Player -> Int -> GameState
drawCards gState playerName cardNum = gState {deck = newDeck, players = newPState} 
  where 
    (newDeck, newPState) = 
      foldl (\(oldDeck, oldPState) _ -> 
        let (newDeck, newCard) = draw oldDeck 
        in (newDeck, addCardToHand oldPState newCard playerName)) 
      (currentDeck, currentPState) [1..cardNum]
    currentDeck = (deck gState)
    currentPState = (players gState)

{- CHOOSING A CARD / DUMB AI -}

{- Given the up card and a list of cards in hand, maybe put down a card -}
dumbPickCard :: (Card, Maybe Color) -> [Card] -> Int -> Maybe (Card, Maybe Color)
dumbPickCard upCard cardList randomNum = 
  if legalCardList /= [] 
    then dumbDecideColor (head legalCardList) randomNum 
    else Nothing 
  where legalCardList = getAllLegalCards cardList upCard

{- Given a card, will return its Maybe (Card, Maybe Color) type -}
dumbDecideColor :: Card -> Int -> Maybe (Card, Maybe Color)
dumbDecideColor Wild randomNum = Just (Wild, Just $ randomColor randomNum)
dumbDecideColor Draw4 randomNum = Just (Wild, Just $ randomColor randomNum)
dumbDecideColor card _ = Just (card, Nothing)

{- Given a random int, will return a color -}
randomColor :: Int -> Color
randomColor num = snd $ foldl (\(rand, chosenColor) color -> 
  if rand == num 
    then (rand + 1, color) 
    else (rand + 1, chosenColor)) 
  (1, Red) [Red, Green, Blue, Yellow]

{- Given a card and a card list, will return the index -}
getCardIndex :: Maybe (Card, Maybe Color) -> [Card] -> Int
getCardIndex (Just (card, _)) cardList = 
  fst $ foldl (\(index, cardNum) x -> 
    if card == x 
      then (cardNum, cardNum + 1) 
      else (index, cardNum + 1)) 
  (0, 0) cardList

{- CHOOSING A CARD / SMART AI -}

{- Picks the best card in the current situation -}
smartPickCard :: GameState -> (Card, Maybe Color) -> [Card] -> Maybe (Card, Maybe Color)
smartPickCard gState upCard cardList
  | compareCards gState (/=) COM2 (<) 3 = dumpHighCostCard cardList legalCardList mostPopularColor
  | compareCards gState (==) COM2 (>) 5 = playHighCostNonWildCard cardList legalCardList mostPopularColor
  | otherwise = holdWild cardList legalCardList mostPopularColor
  where
    legalCardList = getAllLegalCards cardList upCard
    mostPopularColor = getMostPopularColor cardList

{- Checks if a player has a certain number of cards, and returns a boolean -}
compareCards :: GameState -> (Player -> Player -> Bool) -> Player -> (Int -> Int -> Bool) -> Int -> Bool
compareCards gState eqF playerName compF numCards = 
  foldl (\decision pState -> 
    if decision || ((eqF (player pState) playerName) && (compF (length (getCardList gState (player pState))) numCards)) 
      then True 
      else decision) 
  False (players gState)

{- PLAY THE HIGHEST COST CARD -}

{- Given a card list, will play the one with the highest cost first -}
dumpHighCostCard :: [Card] -> [Card] -> Color -> Maybe (Card, Maybe Color)
dumpHighCostCard cardList legalCardList popColor = 
  if legalCardList /= [] 
    then smartDecideColor cardList (snd $ foldl 
      (\(cost, card) aCard -> 
        if cardCost aCard > cost 
          then (cardCost aCard, aCard) 
          else (cost, card)) (-1, (Wild))
      legalCardList) popColor
    else Nothing

{- Same as above, but avoids playing Wild -}
playHighCostNonWildCard :: [Card] -> [Card] -> Color -> Maybe (Card, Maybe Color)
playHighCostNonWildCard cardList legalCardList popColor = 
  if legalCardList /= []
    then if newLegalCardList /= []
      then dumpHighCostCard cardList newLegalCardList popColor
      else dumpHighCostCard cardList legalCardList popColor
    else Nothing
  where newLegalCardList = takeOffWild legalCardList

{- Given a card, will return its respective card cost -}
cardCost :: Card -> Integer
cardCost Wild = 50
cardCost Draw4 = 50
cardCost (Reverse _) = 20
cardCost (Skip _) = 20
cardCost (Draw2 _) = 20
cardCost (Card _ rank) = rank

{- PLAY ANYTHING BUT WILD -}

{- Holds the Wild card only for the last turn -}
holdWild :: [Card] -> [Card] -> Color -> Maybe (Card, Maybe Color)
holdWild cardList legalCardList popColor = 
  if length legalCardList == 1 && checkWild (head legalCardList) 
    then Just (Wild, Just popColor)
    else (playMostPopularColor cardList newLegalCardList popColor) 
  where newLegalCardList = takeOffWild legalCardList

{- Returns True if passed card is Wild -}
checkWild :: Card -> Bool
checkWild Wild = True
checkWild _ = False

{- Filters out the Wild card from the card list -}
takeOffWild :: [Card] -> [Card]
takeOffWild legalCardList = filter (\aCard -> not $ checkWild aCard) legalCardList

{- PLAY THE MOST POPULAR COLOR -}

{- Plays the most popular color given the card list, if there is no such color, will then just play first legal card -}
playMostPopularColor :: [Card] -> [Card] -> Color -> Maybe (Card, Maybe Color)
playMostPopularColor cardList legalCardList popColor =
  if match /= Nothing then match else smartDecideColor cardList (head legalCardList) popColor
  where 
    match = foldl (\bestCard aCard -> 
      if bestCard == Nothing && checkCardColor aCard popColor
        then (smartDecideColor cardList aCard popColor) 
        else bestCard) 
      (Nothing) legalCardList

{- Checks if a card has the same color as the passed color -}
checkCardColor :: Card -> Color -> Bool
checkCardColor (Card col _) matchCol = if col == matchCol then True else False
checkCardColor (Draw2 col) matchCol = if col == matchCol then True else False
checkCardColor (Skip col) matchCol = if col == matchCol then True else False
checkCardColor (Reverse col) matchCol = if col == matchCol then True else False
checkCardColor Wild _ = True
checkCardColor Draw4 _ = True

{- Decide on a color given the most popular color -}
smartDecideColor :: [Card] -> Card -> Color -> Maybe (Card, Maybe Color)
smartDecideColor cardList Wild popColor = Just (Wild, Just popColor)
smartDecideColor cardList Draw4 popColor = Just (Wild, Just popColor)
smartDecideColor _ card _ = Just (card, Nothing)

{- Given a list of cards, returns the color that is the most popular, does not use randomization though... -}
getMostPopularColor :: [Card] -> Color
getMostPopularColor cardList = 
  if r > g 
  then if r > b
    then if r > y then Red else Yellow
    else if b > y then Blue else Yellow
  else if g > b
    then if g > y then Green else Yellow
    else if b > y then Blue else Yellow
  where (r:g:b:y:[]) = getMostPopularColor' cardList

{- Given a card list, will return an int list with the color frequencies -}
getMostPopularColor' :: [Card] -> [Int]
getMostPopularColor' cardList = 
  foldl (\colorList aCard -> getMostPopularColor'' (getCardColor aCard) colorList) [0, 0, 0, 0] cardList

{- Given a card, will update the color list -}
getMostPopularColor'' :: Maybe Color -> [Int] -> [Int]
getMostPopularColor'' Nothing colorList = colorList
getMostPopularColor'' (Just Red) (r:g:b:y:[]) = (r+1:g:b:y:[])
getMostPopularColor'' (Just Green) (r:g:b:y:[]) = (r:g+1:b:y:[])
getMostPopularColor'' (Just Blue) (r:g:b:y:[]) = (r:g:b+1:y:[])
getMostPopularColor'' (Just Yellow) (r:g:b:y:[]) = (r:g:b:y+1:[])
getMostPopularColor'' _ _ = error "getMostPopularColor'': Should not happen"

{- Given a card, will either return its color or nothing -}
getCardColor :: Card -> Maybe Color
getCardColor Wild = Nothing
getCardColor Draw4 = Nothing
getCardColor (Reverse color) = Just color
getCardColor (Skip color) = Just color
getCardColor (Draw2 color) = Just color
getCardColor (Card color _) = Just color

{- DRAWING A NEW CARD -}

{- Given the gamestate and other information, attempt to update the gamestate and maybe another up card -}
drawCard :: GameState -> (Card, Maybe Color) -> Player -> Int -> (GameState, Maybe (Card, Maybe Color))
drawCard gState upCard playerName randomNum = 
  if checkValidity topCard upCard 
    then (gState {deck = restOfDeck}, dumbDecideColor topCard randomNum)
    else (gState {deck = restOfDeck, players = addCardToHand (players gState) topCard playerName}, Nothing)
  where
    (restOfDeck, topCard) = draw (deck gState)

{- ADDING NEW CARD TO HAND -}

{- Given a playerlist, a new card, and a target player, adds the new card to the player's hand and updates the playerlist -}
addCardToHand :: [PlayerState] -> Card -> Player -> [PlayerState]
addCardToHand pStateList newCard playerName = 
  foldl (\pStates state -> 
    if (player state) == playerName 
      then (pStates ++ [addCardToHand' state newCard]) 
      else (pStates ++ [state])) 
  [] pStateList

{- Add a new card to the player's hand -}
addCardToHand' :: PlayerState -> Card -> PlayerState
addCardToHand' pState newCard = let (Hand oldCards) = (hand pState) in pState {hand = (Hand (oldCards ++ [newCard]))}

{-  This functions uses the GameState parameter to determine the status of the game. The function should return a GameStatus 
    based on the following: 

    Active   ==> The current hand for the game is still going (i.e., every player has cards left)
    RoundOver ==> The current hand is done because someone got a "uno" (i.e., a player has no cards left)
    GameOver ==> The game is over because a player has a score of 500 points or more. 
-}

{- Need to call isHandOrGameOver when computer puts down the card -}

{- Returns the game status given a game state-}
isHandOrGameOver :: GameState -> GameStatus 
isHandOrGameOver gState
  | checkWin (players gState) = GameOver
  | checkHand (players gState) = RoundOver
  | otherwise = Active

{- Checks for the game over -}
checkWin :: [PlayerState] -> Bool
checkWin pStateList = 
  foldl (\winCondition pState -> 
    if winCondition || ((score pState) >= 500) 
      then True 
      else False) 
  False pStateList

{- Checks for the round over -}
checkHand :: [PlayerState] -> Bool
checkHand pStateList = 
  foldl (\winCondition pState -> 
    if winCondition || (checkHand' (hand pState)) 
      then True 
      else False) 
  False pStateList

{- Pattern matches the number of cards in the hand -}
checkHand' :: Hand -> Bool
checkHand' (Hand []) = True
checkHand' _ = False

{- 
   Since you (i.e., the student) will represent the human then you already know the rules of the game. Thus, "turnHuman" is function 
   that acts as validity check. The function checks to make sure the card the human wants to place down is valid based on the game 
   rules and the current up card. Here's a description of each parameter to the function: 

   ===== 
   gState:: GameState ==> The current state of the game. You will update this state at some point in this function
   and return a new GameState with those changes. 

   playerCard :: (Int, Maybe Color) ==> This parameter provides the index of card the human wants to place down in its Hand. If the
   card chosen by the human is a "wild" card then the user also provides a color. 

   (card, color) :: (Card, Maybe Color) ==> This parameter represents the current up card. The reason why it's a tuple is that
   the previous player may have placed a "wild" special card down as the up card. This means the previous player must have indicated a color.  
   Thus, the second component is a Maybe value because it may have a Color specified if the card is a "wild" type of card or not. 
    ====== 


   IF the card the human wants to place down is not valid then the function should do the following: 
        1. Deduct two points from the player's score 
        2. Return a tuple with the modified game state and "False" as the second component to indicate the card was invalid 
   ELSE the card is valid and the function should do the following: 
        1. Remove the card from the player's hand 
        2. Return a tuple with the modified game state and "True" as the second component to indicate the card was valid.
-}

{- Human's turn, only needs to check for card validity, remove card, and take off score -}
turnHuman :: GameState ->  (Int, Maybe Color) -> (Card, Maybe Color) -> (GameState, Bool)
turnHuman gState playerCard upCard = 
  if checkValidity (getCardFromHand gState (fst playerCard) Human) upCard
  then (gState {players = (removeCard gState (fst playerCard) Human)}, True) 
  else (gState {players = (deductPoints (players gState) Human)}, False)

{- Gets the hand from the player given the index and player name -}
getCardFromHand :: GameState -> Int -> Player -> Card
getCardFromHand gState index playerName = getCardFromHand' (getCardList gState playerName) index

{- Assume index of card starts at 1! Will return first card in hand if index is not within hand size -}
getCardFromHand' :: [Card] -> Int -> Card
getCardFromHand' cardList index = fst (
  foldl (\(accumCard, countDown) card -> 
    if countDown == 0 
      then (card, countDown - 1)
      else (accumCard, countDown - 1))
  (head cardList, index) cardList)

{- Deducts 2 points from the player and returns the updated [PlayerState] -}
deductPoints :: [PlayerState] -> Player -> [PlayerState]
deductPoints pStateList playerName = 
  map (\pState -> 
    if (player pState) == playerName 
      then deductPoints' pState 
      else pState) 
  pStateList

{- Updates the player state with -2 points -}
deductPoints' :: PlayerState -> PlayerState
deductPoints' pState = pState {score = (score pState) - 2}

{- Removes the card from the player and returns the updated [PlayerState] -}
removeCard :: GameState -> Int -> Player -> [PlayerState]
removeCard gState index playerName = 
  map (\pState -> 
    if (player pState) == playerName 
      then removeCard' pState index 
      else pState) 
  (players gState)

{- Given a player state and a card index, will remove the card -}
removeCard' :: PlayerState -> Int -> PlayerState
removeCard' pState index = pState {hand = (removeCard'' (hand pState) index)}

{- Same as above, but we drill down to the hand instead -}
removeCard'' :: Hand -> Int -> Hand
removeCard'' (Hand cardList) index = fst (
  foldl (\(Hand list, countDown) card -> 
    if countDown == 0 
      then (Hand list, countDown - 1) 
      else (Hand (list ++ [card]), countDown - 1)) 
  (Hand [], index) cardList)

{- Order of checking:

  1. If player plays a wild card                        ->  Always True
  2. If up card is a wild card                          ->  Check Colors only
  3. Both cards are not wild cards                      ->  Check Colors
  4. Both are special cards                             ->  Check if same type
  5. Both are numbered cards                            ->  Check only rank
-}

{- Given a card and an up card, will check whether it is a legal move -}
checkValidity :: Card -> (Card, Maybe Color) -> Bool
checkValidity playerCard (upCard, upColor)
  | playerCard == Wild || playerCard == Draw4 = True
  | upCard == Wild || upCard == Draw4 = if Just (color playerCard) == upColor then True else False
  | otherwise = checkColors playerCard upCard || dynamicCheck playerCard upCard

{- Checks if both the cards have the same color-}
checkColors :: Card -> Card -> Bool
checkColors (Card pcColor _) (Card ucColor _) = if pcColor == ucColor then True else False
checkColors _ _ = False

{- Checks the remaining cases -}
dynamicCheck :: Card -> Card -> Bool
dynamicCheck (Card _ pcRank) (Card _ ucRank) = if pcRank == ucRank then True else False
dynamicCheck (Skip _) (Skip _) = True
dynamicCheck (Reverse _) (Reverse _) = True
dynamicCheck (Draw2 _) (Draw2 _) = True
dynamicCheck _ _ = False