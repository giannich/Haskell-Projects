module Parsing 
(
    parseGrid,
    initSimState,
    detBlueRed,
    parseInt
) where 

import Text.Read

import DataTypes
import SimLogic

------------------
-- FILE PARSING --
------------------

{- Given a list of strings describing the grid, will parse them and return a Simulation State -}
parseGrid :: [String] -> Int -> SimSettings
parseGrid (rowStr:colStr:rest) mr = initSSet
  where
    rowNum = parseInt rowStr
    colNum = parseInt colStr
    emptyState = initSimState (rowNum, colNum) 1 0.5
    filledState = parseRows rest (0, 0) emptyState

    argsPackage = ArgsPackage{maxRounds=mr, rows=rowNum, cols=colNum, neighRadius=1, threshold=0.5, emptyPer=0.2, parksPer=0.1, bluePer=0.5, redPer=0.5}
    initSSet = SimSettings{argsPack=argsPackage, simState=filledState, roundNum=0, stopped=True, stepDelay=1.0}

{- Goes through each row -}
parseRows :: [String] -> Location -> SimState -> SimState
parseRows [] _ oldSS = oldSS
parseRows (row:rows) (rowPtr, colPtr) oldSS = parseRows rows (rowPtr+1, colPtr) newSS
  where
    newSS = parseCols (words row) (rowPtr, colPtr) oldSS

{- Goes through each column -}
parseCols :: [String] -> Location -> SimState -> SimState
parseCols [] _ oldSS = oldSS
parseCols (house:houses) (rowPtr, colPtr) oldSS = parseCols houses (rowPtr, colPtr+1) newSS
  where
    houseState = parseHouse house
    newSS = if houseState /= O then moveIn (rowPtr, colPtr) houseState oldSS else oldSS

{- Parses a string to a state -}
parseHouse :: String -> State
parseHouse "R" = (R Satisfied)
parseHouse "B" = (B Satisfied)
parseHouse "O" = O
parseHouse "P" = P
parseHouse _ = error "There is an undefined state in the grid! Now exiting the simulation..."

-------------------
-- SIMSTATE INIT --
-------------------

{- Given the size of a NxN grid, the radius, and the threshold, initiates a SimState -}
initSimState :: (Int, Int) -> Int -> Float -> SimState
initSimState (rows, cols) rad thresh = SimState {city=initCity rows cols, openHouse=initOpenHouse rows cols, rad=rad, thresh=thresh, elaspedTime=0.0}

{- Given an NxM grid, returns an initialized city -}
initCity :: Int -> Int -> City (Row (House))
initCity rows cols = City [initRow rowNum cols | rowNum <- [0..(rows-1)]]

{- Given a row number and M columns, returns an initialized row -}
initRow :: Int -> Int -> Row (House)
initRow rowNum cols = Row {rowNum=rowNum, houseList=[House {location=(rowNum, colNum), state=O} | colNum <- [0..(cols-1)]]}

{- Given an NxM grid, returns an initialized list of open houses -}
initOpenHouse :: Int -> Int -> OpenHouse (Location)
initOpenHouse rows cols = OpenHouse [(rowNum, colNum) | rowNum <- [0..(rows-1)], colNum <- [0..(cols-1)]]

{- Given a string, returns an Int -}
parseInt :: String -> Int 
parseInt str = case readMaybe str :: Maybe(Int) of 
    Just num -> num 
    Nothing -> error $ "Could not read " ++ str ++ " as an Int!"

{- Given the percentages of empty, blue, and red houses, returns the number of Blue and Red houses -}
{- I tried to make this as arcane as possible, good luck figuring it out -}
detBlueRed :: Int -> Float -> (Float, Float) -> (Int, Int)
detBlueRed tHaus ePer (bPer, _) = (bHaus, rHaus)
  where
    aHaus = (round . (*) ((-) (1.0) ePer) . fromIntegral) tHaus
    bHaus = (round . (*bPer) . fromIntegral) aHaus
    rHaus = (-) aHaus bHaus









