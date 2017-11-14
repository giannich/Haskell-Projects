module Main(
    main 
) where 

import System.Environment
import Text.Read

import DataTypes
import Parsing
import Drawing

{- Main function -}
main :: IO () 
main = do

  args <- getArgs
  chooseDraw args

{- Pattern Matching for determining what to draw -}
chooseDraw :: [String] -> IO ()
chooseDraw (region:[]) = do doDrawWireFrame region
chooseDraw (region:"-w":[]) = do doDrawWireFrame region
chooseDraw (region:"-rgb":year:[]) = do doDrawColor region year drawRGB
chooseDraw (region:"-p":year:[]) = do doDrawColor region year drawPurple
chooseDraw _ = putStrLn $ "Usage: ./Main region [-w | -rgb year | -p year]"

{- Draws a wireframe map given the region name -}
doDrawWireFrame :: String -> IO ()
doDrawWireFrame regName = do

  -- Parsing
  regionData <- readFile (regName ++ ".txt")
  let regionDataLines = lines regionData
  let region = parseRegion regionDataLines

  -- Drawing
  drawWireframe region

{- Draws a colored map given the region name, the year, and the coloring function -}
doDrawColor :: String -> String -> (Region -> [ElectionData] -> IO()) -> IO ()
doDrawColor regName yearStr drawFunc = do

  -- Parsing
  regionData <- readFile (regName ++ ".txt")
  let regionDataLines = lines regionData
  let region = parseRegion regionDataLines

  electionData <- readFile (regName ++ yearStr ++ ".txt")
  let electionDataLines = lines electionData
  let election = parseElectionData electionDataLines

  -- Drawing
  drawFunc region election



