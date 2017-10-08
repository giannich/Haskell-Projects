{- 
  File      :   Recursion.hs 
  Copyright :   (c) Gianni Chen, 10/07/17 
  Sources   :   https://wiki.haskell.org/How_to_work_on_lists
                https://hackage.haskell.org/package/base-4.8.0.0/docs/src/GHC-Base.html#%2B%2B <- source code for "++" operation
                Lec2.hs for length' function
  Notes     :   I created an appendTo function that works in the same way as the "++" operation
                For testing the last exercise, the inputs:

                    let tri1  =  [Point2D [1,1], Point2D [-2,-5,4], Point2D[3,0]]
                    let tri2 = [Point3D [3,4,5],Point2D[2,3], Point2D[1,3] 
                    let tri3  =  [Point2D [1,1], Point2D [-2,-5], Point2D[3,0]]
                    let tri4  =  [Point2D [3,-1], Point2D [2,2], Point2D[-3,-2]]

                should instead instead be:

                    let tri1 = [Point Point2D [1,1], Point Point2D [-2,-5,4], Point Point2D [3,0]]
                    let tri2 = [Point Point3D [3,4,5], Point Point2D[2,3], Point Point2D [1,3]]
                    let tri3 = [Point Point2D [1,1], Point Point2D [-2,-5], Point Point2D [3,0]]
                    let tri4 = [Point Point2D [3,-1], Point Point2D [2,2], Point Point2D [-3,-2]]

                This is because it's expecting "Point PoinTy [a]" instead of only "PoinTy [a]"

  Contents  :   How I passed the latter half of my Saturday doing recursion exercises.
-}

import Data.Typeable

data PointTy = Point2D | Point3D | Point4D 
  deriving (Show, Eq)
data Point a = Point PointTy [a]
  deriving (Show, Eq)

{- Takes in a list and a value and returns a list where the value appears before 
   and after every element in the list. If the input list is empty then the result 
   is a list only containing the value -}
intersperse :: [a] -> a -> [a]
intersperse [] value = [value]
intersperse (x:[]) value = [value, x, value]
intersperse (x:xs) value = value : x : (intersperse xs value)

{- Takes in two lists and weaves the elements of lists together. If either list 
   is exhausted before the other (i.e., the lists do not contain the same number 
   of elements), then add the remaining elements of the non-exhausted list to the 
   returned list -}
weave :: [a] -> [a] -> [a]
weave [] [] = []
weave list [] = list
weave [] list = list
weave (x:xs) (y:ys) = x : y : (weave xs ys)

{- Takes a list of pairs and returns a list of pairs. The returned list matches 
   the input list except that the elements of each pair have had their positions 
   swapped -}
pairSwap :: [(a,b)] -> [(b, a)] 
pairSwap [] = []
pairSwap (x:xs) = (snd x, fst x) : (pairSwap xs)

{- Takes in a list and simply put, reverses the elements in the list. 
   (You cannot use the Prelude reverse in your solution.) -}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = appendTo (reverse' xs) [x]

appendTo :: [a] -> [a] -> [a]
appendTo [] ys = ys
appendTo (x:xs) ys = x : (appendTo xs ys)

{- Takes a list of pairs and returns a pair of lists. The returned pair of lists 
   contains a list of the first elements of the input pair ( a ) and a list of the 
   second elements of the input pairs ( b ). (You cannot use the Prelude unzip in 
   your solution.) -}
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' list = unzip'' list ([],[])

unzip'' :: [(a, b)] -> ([a], [b]) -> ([a], [b])
unzip'' [] accum = accum
unzip'' ((a,b):xs) (accumLeft, accumRight) = unzip'' xs (appendTo accumLeft [a], appendTo accumRight [b])


{- Takes two input lists of a and b and returns a list of ( a , b ) pairs . The 
   returned list is constructed by pairing together respective, by position, 
   elements of the two input lists. If either list is exhausted before the other 
   (i.e., the lists do not contain the same number of elements), then use the "error" 
   function and state that the list has been exhausted. (You cannot use the prelude 
   zip function in your solution.)-}
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' list [] = error "List has been exhausted"
zip' [] lsit = error "List has been exhausted"
zip' (x:xs) (y:ys) = appendTo [(x, y)] (zip' xs ys)

{- Takes a predicate (a function) and a list as arguments. This function must return 
   a pair of lists. The first list in the pair contains all elements of the input list 
   that satisfy the predicate passed to splitFilter. The second list contains all 
   elements that did not satisfy the predicate. The elements in each list must maintain 
   their respective ordering in the original list. -}

splitFilter :: (a -> Bool) -> [a] -> ([a], [a])
splitFilter function [] = ([],[])
splitFilter function list = splitFilter' function list ([],[])

splitFilter' :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
splitFilter' function [] accum = accum
splitFilter' function (x:xs) accum
  | function x == True = splitFilter' function xs (appendTo (fst accum) [x], snd accum)
  | otherwise = splitFilter' function xs (fst accum, appendTo (snd accum) [x])

{- Takes in a point type and a list of points and returns a list all points that match 
   the point type in the input list. -}

findPoint :: PointTy -> [Point a] -> [Point a]
findPoint _ [] = []
findPoint point (x@(Point pointType coords):xs)
  | point == Point2D && pointType == Point2D = x : findPoint point xs
  | point == Point3D && pointType == Point3D = x : findPoint point xs
  | point == Point4D && pointType == Point4D = x : findPoint point xs
  | otherwise = findPoint point xs

{- Takes in a list of points and returns true if every point in the list has the correct 
   number coordinates. A Point2D has only two values, a Point3D contains three values, 
   and a Point4D only contains for 4 values. -}
isValid :: [Point a] -> Bool
isValid [] = True
isValid ((Point pointType coords):xs) 
  | pointType == Point2D && length' coords == 2 = isValid xs
  | pointType == Point3D && length' coords == 3 = isValid xs
  | pointType == Point4D && length' coords == 4 = isValid xs
  | otherwise = False

length' :: [a] -> Integer
length' [] = 0                         
length' (_:xs) = 1 + length' xs

{- Takes in a list of point lists and returns a Maybe list of the indices that represent 
   the index of each point list that creates a right triangle. A valid right triangle can 
   only be composed of three Point2D points. If a point list contains more than three 
   points or has a mixture of other Point types other than Point2D then that point list 
   is not valid (i.e., the function should return "Nothing"). Additionally, the function 
   needs to make sure the Point2D is valid (i.e., it should only contain two values). 
   You can use the Distance Formula and Pythagorean Theorem to determine if the three 
   points make a valid right triangle. -}

findRightTris :: [[Point Double]] -> Maybe [Integer]
findRightTris [] = Nothing
findRightTris list
  | length' (checkEm) > 0 = Just checkEm
  | otherwise = Nothing
  where checkEm = findRightTris' list 0 []

{- Just returns an integer list with indices of Points that make a right triangle.
   Can return an empty list that is evaluated above to Nothing -}
findRightTris' :: [[Point Double]] -> Integer -> [Integer] -> [Integer]
findRightTris' [] _ result = result
findRightTris' (x:xs) index result
  | checkRightTriangle x == True = findRightTris' xs (index + 1) (index : result)
  | otherwise = findRightTris' xs (index + 1) result

{- Functions below here only check if the points make a right triangle -}
checkRightTriangle :: [Point Double] -> Bool
checkRightTriangle ((Point typeI coordsI):(Point typeJ coordsJ):(Point typeK coordsK):[])
  | typeI == Point2D && typeJ == Point2D && typeK == Point2D = pythagoras coordsI coordsJ coordsK
  | otherwise = False
checkRightTriangle [] = False               {- No Points -}
checkRightTriangle (_:[]) = False           {- 1 Point -}
checkRightTriangle (_:_:[]) = False         {- 2 Points -}
checkRightTriangle (_:_:_:_) = False        {- 3+ Points -}

pythagoras :: [Double] -> [Double] -> [Double] -> Bool
pythagoras i j k
  | dist i j == sqrt((dist j k)^2 + (dist k i)^2) = True
  | dist j k == sqrt((dist i j)^2 + (dist k i)^2) = True
  | dist k i == sqrt((dist j k)^2 + (dist i j)^2) = True
  | otherwise = False

dist :: [Double] -> [Double] -> Double
dist (x1:y1:[]) (x2:y2:[]) = sqrt( ((x2-x1)^2) + ((y2-y1)^2) )

dist [] [] = 0
dist _ [] = 0
dist [] _ = 0

dist _ (_:[]) = 0
dist _ (_:_:_:[]) = 0
dist (_:[]) _ = 0
dist (_:_:_:[]) _ = 0

{- Please note that we need to add Point before Point2D, otherwise it doesn't recognize the data
let tri1 = [Point Point2D [1,1], Point Point2D [-2,-5,4], Point Point2D [3,0]]
let tri2 = [Point Point3D [3,4,5], Point Point2D[2,3], Point Point2D [1,3]]
let tri3 = [Point Point2D [1,1], Point Point2D [-2,-5], Point Point2D [3,0]]
let tri4 = [Point Point2D [3,-1], Point Point2D [2,2], Point Point2D [-3,-2]]
-}