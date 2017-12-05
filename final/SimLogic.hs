module SimLogic 
(
    getSatisfied,
    updateSimState,
    moveInPeople,
    moveIn
) where 

import System.Random

import DataTypes

----------------
-- INIT LOGIC --
----------------

{- Given a random generator and a specific state, moves in the person in a random empty house -}
moveInPeople :: Int -> State -> StdGen -> SimState -> SimState
moveInPeople 0 _ _ ss = ss
moveInPeople num st gen oldSS = moveInPeople (num - 1) st gen' newSS
  where
    (randNum, gen') = random gen :: (Int, StdGen)
    loc = pickRandLoc randNum (openHouse oldSS)
    newSS = moveIn loc st oldSS

{- Given a location and a state, moves the state into the empty location and removes it from the open house list -}
moveIn :: Location -> State -> SimState -> SimState
moveIn loc st ss = ss{city=newCity, openHouse=newOpenHouse}
  where
    newCity = fmap (changeState loc st) (city ss)
    newOpenHouse = removeFromOpenHouse loc (openHouse ss)

{- Given a number, returns the index of the list modulo the length -}
pickRandLoc :: Int -> OpenHouse (Location) -> Location
pickRandLoc randNum openHouse = fst $ foldl chooseLoc ((0, 0), mod randNum (length openHouse)) openHouse

{- Folding function -}
chooseLoc :: (Location, Int) -> Location -> (Location, Int)
chooseLoc (accum, count) loc = if count == 0 then (loc, count - 1) else (accum, count - 1)

-------------------
-- GENERAL FUNCS --
-------------------

{- Gets the percentage of satisfied households -}
getSatisfied :: SimState -> Float
getSatisfied ss = sat / (nonSat + sat) * 100
  where
    (sat, nonSat) = foldl (\sum rows -> foldl countSatisfaction sum rows) (0.0, 0.0) (city ss)

{- Folding function -}
countSatisfaction :: (Float, Float) -> House -> (Float, Float)
countSatisfaction sats house = isSatisfied sats (state house)

{- Pattern matches the satisfied and unsatisfied households and updates the count -}
isSatisfied :: (Float, Float) -> State -> (Float, Float)
isSatisfied (sat, nonSat) (B Satisfied) = (sat + 1, nonSat)
isSatisfied (sat, nonSat) (R Satisfied) = (sat + 1, nonSat)
isSatisfied (sat, nonSat) (B Unsatisfied) = (sat, nonSat + 1)
isSatisfied (sat, nonSat) (R Unsatisfied) = (sat, nonSat + 1)
isSatisfied sats _ = sats

{- Updates the state of the simulation -}
updateSimState :: SimState -> SimState
updateSimState oldSS = foldl traverseRows oldSS (city oldSS)

{- Traverse Rows -}
traverseRows :: SimState -> Row (House) -> SimState
traverseRows oldSS singleRow = foldl traverseCols oldSS singleRow

{- Traverse Cols -}
traverseCols :: SimState -> House -> SimState
traverseCols oldSS oldHouse = (checkSatisfaction . flipHouseState oldSS) oldHouse

{- First checks for similarity -}
flipHouseState :: SimState -> House -> (House, SimState)
flipHouseState oldSS oldHouse@(House _ O) = (oldHouse, oldSS)
flipHouseState oldSS oldHouse@(House _ P) = (oldHouse, oldSS)
flipHouseState oldSS oldHouse@(House houseLoc houseState) = (updatedHouse, updatedSS)
  where
    t = thresh oldSS
    simScore = checkSimilarity oldSS houseState houseLoc (+0)
    updatedState = if simScore < t then (changeSatisfaction houseState) else houseState
    updatedHouse = oldHouse{state=updatedState}
    updatedCity = fmap (changeState houseLoc updatedState) (city oldSS)
    updatedSS = oldSS{city=updatedCity}

{- Then checks for satisfaction -}
checkSatisfaction :: (House, SimState) -> SimState
checkSatisfaction (updatedHouse@(House l (B Unsatisfied)), updatedSS) = checkNewLocations updatedHouse updatedSS
checkSatisfaction (updatedHouse@(House l (R Unsatisfied)), updatedSS) = checkNewLocations updatedHouse updatedSS
checkSatisfaction (_, updatedSS) = updatedSS

{- And finally checks for new locations if not satisfied-}
checkNewLocations :: House -> SimState -> SimState
checkNewLocations updatedHouse updatedSS = 
  if maybeNewLoc /= Nothing
      then swapLoc (location updatedHouse) (extractLocation maybeNewLoc) updatedSS
      else updatedSS
  where
    maybeNewLoc = attendOpenHouse updatedSS (state updatedHouse)

{- Extracts the loction  -}
extractLocation :: Maybe (Location) -> Location
extractLocation (Just loc) = loc
extractLocation Nothing = error $ "wtf should not happen"

{- Turn unsatisfied in case of being below the satisfaction threshold -}
changeSatisfaction :: State -> State
changeSatisfaction (B Satisfied) = B Unsatisfied
changeSatisfaction (R Satisfied) = R Unsatisfied
changeSatisfaction same = same

------------------------
-- COMPUTE SIMILARITY --
------------------------

{- Given a location in a city and the simulation state, returns the similarity score -}
checkSimilarity :: SimState -> State -> Location -> (Int -> Int) -> Float
checkSimilarity ss compState loc includeSelf = numerator / denominator
  where
    Neighborhood n = getNeighborhood loc ss
    -- Get number of houses that match the same state as the comparing state and the number of occupied houses
    numerator = (fromIntegral . includeSelf . length) $ filter (isSameState compState . state) n
    denominator = (fromIntegral . includeSelf . length) $ filter (isOccupied . state) n

{- Given a loction in a city and the simulation state, constructs a neighborhood around the location -}
getNeighborhood :: Location -> SimState -> Neighborhood (House)
getNeighborhood (row, col) ss = Neighborhood houses
  where
    r = (-) (rad ss) (1) -- Minus 1 because a radius of 1 means having a range of 0
    City rowList = city ss

    rows = filter (checkRange row r . rowNum) rowList
    houses = foldl (\accum x -> accum ++ (filter (checkRange col r . snd . location) $ houseList x)) [] rows

{- Given a test integer, checks if it is within the base +/- radius range -}
checkRange :: Int -> Int -> Int -> Bool
checkRange base radius test = if test >= (base - radius) && test <= (base + radius) then True else False

{- Given two states, returns whether they are same states, if a state is compared to a park, it will always return true -}
isSameState :: State -> State -> Bool
isSameState _ P = True
isSameState (B _) (B _) = True
isSameState (R _) (R _) = True
isSameState _ _ = False

{- Given a state, returns whether it's occupied or not -}
isOccupied :: State -> Bool
isOccupied O = False
isOccupied _ = True

--------------------
-- SWAP LOCATIONS --
--------------------

{- Given two locations and the simState, will swap the state data -}
swapLoc :: Location -> Location -> SimState -> SimState
swapLoc oldLoc newLoc oldSS = oldSS {city=newCity, openHouse=newOpenHouse}
  where
    -- Creates a new state depending on the older state
    oldState = state $ getHouse oldLoc (city oldSS)
    newState = if oldState == B Unsatisfied then B Satisfied else R Satisfied

    newCity = fmap (changeState oldLoc O . changeState newLoc newState) (city oldSS) 
    newOpenHouse = (addToOpenHouse oldLoc . removeFromOpenHouse newLoc) (openHouse oldSS)

{- Given a location in a city, returns the house in question -}
getHouse :: Location -> City (Row (House)) -> House
getHouse loc@(row, col) (City rowList) = houseObj
  where
    rowObj = head $ filter (\x -> (rowNum x) == row) rowList
    houseObj = head $ filter (\x -> (location x) == loc) (houseList rowObj)

{- Given a location, a new state and the old state of the rows, will return the updated row state -}
-- FUNCTOR HERE
changeState :: Location -> State -> Row (House) -> Row (House)
changeState loc newState oldRow = fmap (changeHouseState loc newState) oldRow

{- If the location of the house matches the given location, will return a house with the changed state -}
changeHouseState :: Location -> State -> House -> House
changeHouseState loc newState house = if (location house) == loc then house{state=newState} else house

{- Give an open house and a location, will return the list of locations without the specified location -}
removeFromOpenHouse :: Location -> OpenHouse (Location) -> OpenHouse (Location)
removeFromOpenHouse loc (OpenHouse locationList) = OpenHouse (filter (/= loc) locationList)

{- Give an open house and a location, will return the list of locations plus the specified location -}
addToOpenHouse :: Location -> OpenHouse (Location) -> OpenHouse (Location)
addToOpenHouse loc (OpenHouse locationList) = OpenHouse (locationList ++ [loc])

--------------------------
-- EVALUATE OPEN HOUSES --
--------------------------

{- Unsatisfied owner shops for a new house -}
attendOpenHouse :: SimState -> State -> Maybe (Location)
attendOpenHouse ss compState = fst $ foldl (attendOpenHouse' ss compState) (Nothing, 2.0) (openHouse ss)

attendOpenHouse' :: SimState -> State -> (Maybe Location, Float) -> Location -> (Maybe Location, Float)
attendOpenHouse' ss compState (mLoc, minSim) x = 
  if simScore > t && simScore < minSim 
    then (Just x, simScore) 
    else (mLoc, minSim) 
  where 
    t = thresh ss
    simScore = checkSimilarity ss compState x (+1)

