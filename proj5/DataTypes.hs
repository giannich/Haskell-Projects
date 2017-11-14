module DataTypes 
(
    Point(..),
    BoundingBox(..), 
    Subregion(..),
    Region(..),
    ElectionData(..)
) where 

import Graphics.Gloss.Data.Picture

{- Data for a Region -}
data Region = Region {  boundingBox :: BoundingBox,
                        subregionNum :: Int,
                        subregions :: [Subregion]}
                        deriving (Eq)

{- Data for a Subregion -}
data Subregion = Subregion {  name :: String,
                              parentRegion :: String,
                              pointNum :: Int,
                              points :: [Point]}
                              deriving (Eq)

{- Data for a boundingbox in terms of (Min Point, Max Point) -}
data BoundingBox = BoundingBox {  minPoint :: Point, 
                                  maxPoint :: Point} 
                                  deriving (Eq)

{- Data for an ElectionData -}
data ElectionData = ElectionData {  regionName :: String,
                                    romney :: Int,
                                    obama :: Int,
                                    other :: Int}
                                    deriving (Show, Eq)

instance Show Region where
  show region = "\nRegion Bounding Box: " ++ (show $ boundingBox region) ++
                "\n\nSubregions Number: " ++ (show $ subregionNum region) ++
                "\nSubregions: " ++ (show $ subregions region)

instance Show Subregion where
  show subregion = "\n\nSubregion Name: " ++ (name subregion) ++
                   "\nParent Region: " ++ (parentRegion subregion) ++
                   "\nPoints Number: " ++ (show $ pointNum subregion) ++
                   "\nPoints: " ++ (show $ points subregion)

instance Show BoundingBox where
  show box = "\nMin Point: " ++ (show $ fst min) ++ "\t" ++ (show $ snd min) ++
             "\nMax Point: " ++ (show $ fst max) ++ "\t" ++ (show $ snd max)
             where
              min = minPoint box
              max = maxPoint box
