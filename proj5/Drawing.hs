module Drawing 
(
    drawWireframe,
    drawRGB,
    drawPurple
) where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display

import DataTypes
import Parsing

windowWidth = 1024
windowHeight = 768

{- WIREFRAME MAP -}

{- Given a region, will draw its shape with a wireframe -}
drawWireframe :: Region -> IO()
drawWireframe region = 
  displayIO window white (
    return $ 
      normalizePicture region (
        renderWireframe region)) 
  (\_ -> return ())

{- Window Display -}
window :: Display
window = InWindow "Purple America" (windowWidth, windowHeight) (10, 10)

{- Renders the Region into a wireframe Picture -}
renderWireframe :: Region -> Picture
renderWireframe region = pictures (foldl (dynamicFoldFunc subregionToLine) [] (subregions region))

{- Normalizes the Picture given the bounding box constraint -}
normalizePicture :: Region -> Picture -> Picture
normalizePicture region oldPic = 
  scale scaleFactor scaleFactor (
    translate transHor transVer (oldPic))
  where
    BoundingBox (minX, minY) (maxX, maxY) = boundingBox region
    (xDist, yDist) = (maxX - minX, maxY - minY)
    (transHor, transVer) = (-(minX + xDist / 2), -(minY + yDist / 2))
    scaleFactor = determineScale xDist yDist

{- Determines the scaling factor -}
determineScale :: Float -> Float -> Float
determineScale xDist yDist
  | scaleX < scaleY = scaleX
  | otherwise = scaleY
  where
    scaleX = (fromIntegral windowWidth :: Float) / xDist
    scaleY = (fromIntegral windowHeight :: Float) / yDist

{- Creates a Picture from a Subregion -}
subregionToLine :: Subregion -> Picture
subregionToLine subregion = lineLoop (points subregion)

{- RGB MAP -}

{- Given a region and election data, will draw its shape and color it red or blue -}
drawRGB :: Region -> [ElectionData] -> IO()
drawRGB region electionDataList = 
  displayIO window white (
    return $ 
      normalizePicture region (
        renderPolygon region electionDataList determineRBGColor)) 
  (\_ -> return ())

{- Renders the Region into a polygon Picture -}
renderPolygon :: Region -> [ElectionData] -> (Subregion -> [ElectionData] -> Color) -> Picture
renderPolygon region electionDataList colorFunc = 
  pictures [
    pictures (
      foldl (dynamicFoldFunc $ subregionToPolygon colorFunc electionDataList) [] (subregions region)), 
    renderWireframe region]

{- Given a color and a region, returns a function that colors a picture -}
subregionToPolygon :: (Subregion -> [ElectionData] -> Color) -> [ElectionData] -> Subregion -> Picture
subregionToPolygon colorFunc electionDataList subregion = (color (colorFunc subregion electionDataList) . polygon) $ points subregion

{- Given the subregion and the election data, returns an RGB color -}
determineRBGColor :: Subregion -> [ElectionData] -> Color
determineRBGColor subregion electionDataList
  | rep > dem = red
  | rep < dem = blue
  | otherwise = green
  where
    elecData = getElecData subregion electionDataList
    rep = romney elecData 
    dem = obama elecData

{- Given the subregion and the list of ElectionData, will return the ElectionData matching the subregion -}
getElecData :: Subregion -> [ElectionData] -> ElectionData
getElecData subregion electionDataList = head $ filter (\x -> if regionName x == (name subregion) then True else False) electionDataList

{- PURPLE MAP -}

{- Given a region and election data, will draw its shape in purple -}
drawPurple :: Region -> [ElectionData] -> IO()
drawPurple region electionDataList = 
  displayIO window white (
    return $ 
      normalizePicture region (
        renderPolygon region electionDataList determinePurpleColor)) 
  (\_ -> return ())

{- Given the subregion and the election data, returns an RGB color -}
determinePurpleColor :: Subregion -> [ElectionData] -> Color
determinePurpleColor subregion electionDataList =
  makeColor r g b 1.0
  where
    elecData = getElecData subregion electionDataList
    rep = fromIntegral (romney elecData) :: Float
    dem = fromIntegral (obama elecData) :: Float
    otr = fromIntegral (other elecData) :: Float
    tot = rep + dem + otr
    r = rep / tot
    g = otr / tot
    b = dem / tot
