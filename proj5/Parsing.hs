module Parsing 
(
    parseRegion,
    dynamicFoldFunc,
    parseElectionData
) where 

import Data.List.Split
import Text.Read

import DataTypes

{- Given a list of strings, parses all the strings and returns a Region -}
parseRegion :: [String] -> Region
parseRegion (minLongLat:maxLongLat:subregionNumsStr:_:regionStr) = 
  Region {
    boundingBox = parseBoundingBox minLongLat maxLongLat, 
    subregionNum = parseInt subregionNumsStr, 
    subregions = parseSubregions regionStr}

{- Given two strings with the coordinates, returns a BoundingBox -}
parseBoundingBox :: String -> String -> BoundingBox
parseBoundingBox minLongLat maxLongLat = 
  BoundingBox {
    minPoint = parsePoint minLongLat,
    maxPoint = parsePoint maxLongLat}

{- Given a list of strings and an int, returns a list of subregions -}
parseSubregions :: [String] -> [Subregion]
parseSubregions strings = foldl (dynamicFoldFunc parseSubregions') [] (splitOn [""] strings)

{- Given a list of strings, returns a subregion -}
parseSubregions' :: [String] -> Subregion
parseSubregions' (subregName:regName:pointStr:rest) = 
  Subregion {
  name = subregName, 
  parentRegion = regName, 
  pointNum = parseInt pointStr, 
  points = parsePoints rest}

{- Parse a list of coordinates into a list of points -}
parsePoints :: [String] -> [Point]
parsePoints strings = foldl (dynamicFoldFunc parsePoint) [] strings

{- Given a string, returns a Point -}
parsePoint :: String -> Point
parsePoint pointStr = (parseFloat lon, parseFloat lat)
  where 
    (lon:lat:[]) = words pointStr

{- Given a string, returns a Float -}
parseFloat :: String -> Float
parseFloat str = case readMaybe str :: Maybe(Float) of 
    Just num -> num 
    Nothing -> error $ "Could not read " ++ str ++ " as a Float!"

{- Given a string, returns an Int -}
parseInt :: String -> Int 
parseInt str = case readMaybe str :: Maybe(Int) of 
    Just num -> num 
    Nothing -> error $ "Could not read " ++ str ++ " as an Int!"

{- Given a parsing function, returns a function to be used for foldl -}
dynamicFoldFunc :: (b -> a) -> ([a] -> b -> [a])
dynamicFoldFunc f = (\aList b -> aList ++ [f b])

{- Given a list of strings, parses all the strings and returns a list of ElectionData -}
parseElectionData :: [String] -> [ElectionData]
parseElectionData (header:rest) = foldl (dynamicFoldFunc parseElectionData') [] rest

{- Given a string, returns an ElectionData type -}
parseElectionData' :: String -> ElectionData
parseElectionData' string = 
  ElectionData {
  regionName = name, 
  romney = parseInt rep, 
  obama = parseInt dem, 
  other = parseInt oth}
  where
    (name:rep:dem:oth:_:[]) = splitOn "," string
