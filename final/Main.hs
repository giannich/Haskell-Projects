module Main(
    main 
) where 

import Text.Read
import System.Random
import System.Environment

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Game as Game

import Parsing
import Drawing
import SimLogic
import DataTypes

--------------
-- SETTINGS --
--------------

{- Min and Max Parameters -}
thresholdRange = (0.0, 1.0)
radiusRange = (1, 5)
delayRange = (0.5, 3.0)
redBlueRange = (0.0, 1.0)
emptyRange = (0.0, 1.0)
parksRange = (0.0, 1.0)
sizeRange = (10, 50)

{- Single Step Values -}
thresholdStep = 0.05
radiusStep = 1
delayStep = 0.100
redBlueStep = 0.05
emptyStep = 0.05
parkStep = 0.05
smallSizeStep = 1
bigSizeStep = 5

{- Size of actual window -}
windowWidth = 1024
windowHeight = 768

{- Window Display -}
window :: Display
window = InWindow "Schelling" (windowWidth, windowHeight) (10, 10)

{- FPS -}
fps :: Int 
fps = 60 

{- Default args -}
initArgs :: Int -> ArgsPackage
initArgs mr = 
  ArgsPackage{
  maxRounds=mr,
  rows=30, 
  cols=30, 
  neighRadius=2,
  threshold=0.5, 
  emptyPer=0.2,
  parksPer=0.0,
  bluePer=0.5, 
  redPer=0.5}

----------
-- MAIN --
----------

{- Main Function -}
main :: IO()
main = do
  -- Get args
  args <- getArgs
  chooseArgs args

---------------
-- ARGUMENTS --
---------------

{- Pattern matching for arguments -}
chooseArgs :: [String] -> IO ()

-- Matches only the max steps
chooseArgs (mr:[]) = do
  putStrLn $ "Usage: ./Main maxRounds [-f fileName]"
  (gen, gen') <- getGens
  let argsPackage = initArgs $ parseInt mr
  let fullSS = initState argsPackage gen gen'
  let initSimSettings = SimSettings{argsPack=argsPackage, simState=fullSS, roundNum=0, stopped=True, stepDelay=1.0}

  -- Play the simulation
  playIO window white fps initSimSettings drawSimSettings eventHandler stepFunc

-- Matches for a file input
chooseArgs (mr:"-f":fileName:[]) = do
  putStrLn $ "Usage: ./Main maxRounds [-f fileName]"
  gridData <- readFile fileName
  let gridDataLines = lines gridData
  let initSimSettings = parseGrid gridDataLines (parseInt mr)

  -- Play the simulation
  playIO window white fps initSimSettings drawSimSettings eventHandler stepFunc

-- Doesn't Match anything
chooseArgs _ = putStrLn $ "Usage: ./Main maxRounds [-f fileName]"

{- Given an args package and 2 different stdgens, inits the sim state -}
initState :: ArgsPackage -> StdGen -> StdGen -> SimState
initState (ArgsPackage mr rowNum colNum neighRadius threshold emptyPer parksPer bluePer redPer) gen gen' = fullSS
  where
    emptySS = initSimState (rowNum, colNum) neighRadius threshold
    totalPlots = rowNum * colNum

    -- Move in Parks first
    parks = (round . (*) parksPer . fromIntegral) totalPlots
    parkSS = moveInPeople parks P gen' emptySS

    -- Then move in the rest of the red and blue houses
    (blueHouses, redHouses) = detBlueRed (totalPlots - parks) emptyPer (bluePer, redPer)
    halfSS = moveInPeople blueHouses (B Satisfied) gen parkSS
    fullSS = moveInPeople redHouses (R Satisfied) gen' halfSS

--------------------
-- EVENT HANDLING --
--------------------

{- For handling events -}
eventHandler :: Event -> SimSettings -> IO (SimSettings)

-- Start / Stop
eventHandler (EventKey (SpecialKey KeyEnter) Up _ _) sSet = return $ sSet{stopped = (not . stopped) sSet}

-- Reset
eventHandler (EventKey (SpecialKey KeyLeft) Up _ _) sSet = resetState sSet (argsPack sSet)

-- Step
eventHandler (EventKey (SpecialKey KeyRight) Up _ _) sSet@(SimSettings _ ss round _ _) = 
  return $ sSet{simState = updateSimState ss, roundNum = round + 1}

-- Exit
eventHandler (EventKey (SpecialKey KeyEsc) Up _ _) _ = error $ "Thanks for playing with Schelling!"

-- Change Similarity Threshold
eventHandler (EventKey (Game.Char 'q') Up _ _) sSet = changeThresh (-thresholdStep) sSet
eventHandler (EventKey (Game.Char 'w') Up _ _) sSet = changeThresh (thresholdStep) sSet

-- Change Neighborhood Radius Threshold
eventHandler (EventKey (Game.Char 'a') Up _ _) sSet = changeRad (-radiusStep) sSet
eventHandler (EventKey (Game.Char 's') Up _ _) sSet = changeRad (radiusStep) sSet

-- Change Time Delay
eventHandler (EventKey (Game.Char 'z') Up _ _) sSet = changeDelay (-delayStep) sSet
eventHandler (EventKey (Game.Char 'x') Up _ _) sSet = changeDelay (delayStep) sSet

-- Change R/B Percentages
eventHandler (EventKey (Game.Char 'r') Up _ _) sSet = changeRB ((redBlueStep), (-redBlueStep)) sSet
eventHandler (EventKey (Game.Char 't') Up _ _) sSet = changeRB ((-redBlueStep), (redBlueStep)) sSet

-- Change Empty Percentages
eventHandler (EventKey (Game.Char 'f') Up _ _) sSet = changeEmpty (-emptyStep) sSet
eventHandler (EventKey (Game.Char 'g') Up _ _) sSet = changeEmpty (emptyStep) sSet

-- Change Time Delay
eventHandler (EventKey (Game.Char 'v') Up _ _) sSet = changeParks (-parkStep) sSet
eventHandler (EventKey (Game.Char 'b') Up _ _) sSet = changeParks (parkStep) sSet

-- Change Size
eventHandler (EventKey (Game.Char 'u') Up _ _) sSet = changeSize (-bigSizeStep) sSet
eventHandler (EventKey (Game.Char 'i') Up _ _) sSet = changeSize (-smallSizeStep) sSet
eventHandler (EventKey (Game.Char 'o') Up _ _) sSet = changeSize (smallSizeStep) sSet
eventHandler (EventKey (Game.Char 'p') Up _ _) sSet = changeSize (bigSizeStep) sSet

-- Invalid Input
eventHandler _ sSet = return sSet

{- For resetting the state -}
resetState :: SimSettings -> ArgsPackage -> IO (SimSettings)
resetState sSet newArgs = do
  (gen, gen') <- getGens
  return $ sSet{simState=initState newArgs gen gen', argsPack=newArgs, roundNum=0, stopped=True, stepDelay=1.0}

getGens :: IO (StdGen, StdGen)
getGens = do
  gen <- getStdGen 
  gen' <- newStdGen
  return (gen, gen')

{- Directly creates a new SimSettings from a change in threshold, does not reset sim -}
changeThresh :: Float -> SimSettings -> IO (SimSettings)
changeThresh change sSet@(SimSettings args ss _ _ _) = do
  let newThresh = changeFloat (threshold args) change thresholdRange
  return sSet{argsPack=args{threshold=newThresh}, simState=ss{thresh=newThresh}}

changeFloat :: Float -> Float -> (Float, Float) -> Float
changeFloat old change (min, max) = if old + change > max then max else if old + change < min then min else old + change

{- For creating a new args package for neighborhood radius change -}
changeRad :: Int -> SimSettings -> IO (SimSettings)
changeRad change sSet@(SimSettings args _ _ _ _) = resetState sSet args{neighRadius=changeInt (neighRadius args) change radiusRange}

changeInt :: Int -> Int -> (Int, Int) -> Int
changeInt old change (min, max) = if old + change > max then max else if old + change < min then min else old + change

{- Directly creates a new SimSettings from a change in threshold, does not reset sim -}
changeDelay :: Float -> SimSettings -> IO (SimSettings)
changeDelay change sSet@(SimSettings _ _ _ _ delay) = return sSet{stepDelay=changeFloat delay change delayRange}

{- For creating a new args package for r/b change -}
changeRB :: (Float, Float) -> SimSettings -> IO (SimSettings)
changeRB change sSet@(SimSettings args _ _ _ _) = do
  let (newRed, newBlue) = changeRedBlue (redPer args, bluePer args) change redBlueRange
  let newArgs = args{redPer=newRed, bluePer=newBlue}
  resetState sSet newArgs

changeRedBlue :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
changeRedBlue (oldRed, oldBlue) (changeRed, changeBlue) (min, max) = 
  if newRed > max || newBlue > max || newRed < min || newBlue < min
    then (oldRed, oldBlue) 
    else (newRed, newBlue)
  where
    (newRed, newBlue) = (oldRed + changeRed, oldBlue + changeBlue)

{- For creating a new args package for empty change -}
changeEmpty :: Float -> SimSettings -> IO (SimSettings)
changeEmpty change sSet@(SimSettings args _ _ _ _) = resetState sSet args{emptyPer=changeFloat (emptyPer args) change emptyRange}

{- For creating a new args package for parks change -}
changeParks :: Float -> SimSettings -> IO (SimSettings)
changeParks change sSet@(SimSettings args _ _ _ _) = resetState sSet args{parksPer=changeFloat (parksPer args) change parksRange}

{- For creating a new args package for size change -}
changeSize :: Int -> SimSettings -> IO (SimSettings)
changeSize change sSet@(SimSettings args _ _ _ _) = do 
  let (newRows, newCols) = (changeInt (rows args) change (10, 50), changeInt (cols args) change sizeRange)
  let newArgs = args{rows=newRows, cols=newCols}
  resetState sSet newArgs

-------------------
-- STEP FUNCTION --
-------------------

{- Step function for each second -}
stepFunc :: Float -> SimSettings -> IO (SimSettings)
stepFunc deltaTime sSet@(SimSettings args ss round stop delay) =
  if round >= (maxRounds args) || stop then return sSet{stopped=True} else
  if eTime' > delay 
    then return $ checkSim sSet
    else return $ sSet{simState=changeTimer eTime' ss}
  where
    eTime' = (elaspedTime ss) + deltaTime

{- This also checks if the simstates are the same after the simulation update -}
checkSim :: SimSettings -> SimSettings
checkSim oldSSet@(SimSettings args ss round stop _) = 
  if city ss == (city . simState) newSSet
    then sameSSet
    else newSSet
  where
    newSSet = oldSSet{simState=(changeTimer 0.0 . updateSimState) ss, roundNum=round+1}
    sameSSet = oldSSet{stopped=True}

{- Given a time and a SimState, returns a new SimState with a new time -}
changeTimer :: Float -> SimState -> SimState
changeTimer newTime ss = ss{elaspedTime=newTime}









