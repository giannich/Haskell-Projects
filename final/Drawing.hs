module Drawing 
(
    drawSimSettings
) where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display

import DataTypes
import SimLogic

{- Size of the grid -}
gridWidth = 800
gridHeight = 600

-----------------
-- SIMSETTINGS --
-----------------

{- Given SimSettings, will draw the arguments and the simstate -}
drawSimSettings :: SimSettings -> IO (Picture)
drawSimSettings sSet@(SimSettings args ss _ _ _) = return $ pictures $ (drawArgs sSet) : [drawSimState (rows args, cols args) ss]

{- Given a SimState, will draw its shape -}
drawSimState :: (Int, Int) -> SimState -> Picture
drawSimState (n, m) ss = (translate 150 0 . normalizePicture (n, m) . renderSimState) ss

------------------
-- ARGS PICTURE --
------------------

{- Given an ArgsPackage, will render the info from the argsPackage -}
drawArgs :: SimSettings -> Picture
drawArgs (SimSettings args ss round stop delay) = pictures $ [num, sat, sim, nei, rbp, emp, par, siz, del, sta, res, ste, exi, sst]
  where
    num = (normalizeArg (280) . text) ("Round #: \t" ++ (show round) ++ "/" ++ (show $ maxRounds args))
    sat = (normalizeArg (240) . text) ("Satisfaction: \t" ++ (show $ getSatisfied ss) ++ "%")

    sim = (normalizeArg (160) . text) ("[Q] [W] Similarity: \t" ++ (show $ threshold args))
    nei = (normalizeArg (120) . text) ("[A] [S] Radius: \t" ++ (show $ rad ss))
    rbp = (normalizeArg (80) . text) ("[R] [T] Red/Blue: \t" ++ (show $ redPer args * 100) ++ "%/" ++ (show $ bluePer args * 100) ++ "%")
    emp = (normalizeArg (40) . text) ("[F] [G] Empty: \t\t" ++ (show $  emptyPer args * 100) ++ "%")
    par = (normalizeArg (0) . text) ("[V] [B] Parks: \t\t" ++ (show $  parksPer args * 100) ++ "%")
    siz = (normalizeArg (-40) . text) ("[U] [I] [O] [P] Size: \t" ++ (show $ rows args) ++ "x" ++ (show $ cols args))
    del = (normalizeArg (-80) . text) ("[Z] [X] Delay: \t" ++ (show $ delay * 1000) ++ "ms")

    sta = (normalizeArg (-160) . text) ("[Enter] Start / Stop")
    res = (normalizeArg (-200) . text) ("[Left] Reset")
    ste = (normalizeArg (-240) . text) ("[Right] Step")
    exi = (normalizeArg (-280) . text) ("[ESC] Exit")

    stopStr = if stop then "Stopped" else "Playing"
    sst = (translate (30) (-340) . scale 0.15 0.15 . text) ("Simulation State: " ++ stopStr)

normalizeArg :: Float -> Picture -> Picture
normalizeArg transVer oldPic = (translate (-500) transVer . scale 0.15 0.15) oldPic

----------------------
-- SIMSTATE PICTURE --
----------------------

{- SimState grid -}
renderSimState :: SimState -> Picture
renderSimState ss = pictures (foldl (dynamicFoldFunc rowToPictures) [] rowList)
  where
    City rowList = city ss

{- Given a row, renders a bunch of pictures -}
rowToPictures :: Row (House) -> Picture
rowToPictures row = pictures (foldl (dynamicFoldFunc houseToPolygon) [] (houseList row))

{- Given a house, returns a colored polygon with a black border -}
houseToPolygon :: House -> Picture
houseToPolygon house = pictures [(color coloring . polygon) path, lineLoop path]
  where
    path = locationToPath $ location house
    coloring = deterMineColor $ state house

{- Given a location, returns a path -}
locationToPath :: Location -> Path
locationToPath (xInt, yInt) = [(x, y), (x + 1, y), (x + 1, y + 1), (x, y + 1)]
  where
    x = fromIntegral xInt
    y = fromIntegral yInt

{- Given a state, returns its color -}
deterMineColor :: State -> Color
deterMineColor (B Satisfied) = makeColor 0.0 0.0 1.0 1.0
deterMineColor (B Unsatisfied) = makeColor 0.0 0.0 1.0 0.5
deterMineColor (R Satisfied) = makeColor 1.0 0.0 0.0 1.0
deterMineColor (R Unsatisfied) = makeColor 1.0 0.0 0.0 0.5
deterMineColor O = white
deterMineColor P = makeColor 0.0 0.75 0.0 1.0

{- Normalizes the Picture given the bounding box constraint -}
normalizePicture :: (Int, Int) -> Picture -> Picture
normalizePicture (maxXInt, maxYInt) oldPic = (rotate 90.0 . scale scaleFactor scaleFactor . translate transHor transVer) oldPic
  where
    (maxX, maxY) = (fromIntegral maxXInt, fromIntegral maxYInt)
    (minX, minY) = (0.0, 0.0)
    (xDist, yDist) = (maxX - minX, maxY - minY)
    (transHor, transVer) = (-(minX + xDist / 2), -(minY + yDist / 2))
    scaleFactor = determineScale xDist yDist

{- Determines the scaling factor -}
determineScale :: Float -> Float -> Float
determineScale xDist yDist
  | scaleX < scaleY = scaleX
  | otherwise = scaleY
  where
    scaleX = (fromIntegral gridWidth) / xDist
    scaleY = (fromIntegral gridHeight) / yDist

{- Given a parsing function, returns a function to be used for foldl -}
dynamicFoldFunc :: (b -> a) -> ([a] -> b -> [a])
dynamicFoldFunc f = (\aList b -> aList ++ [f b])








