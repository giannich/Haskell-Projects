{- 
  File      :   HigherOrder.hs 
  Copyright :   (c) Gianni Chen, 10/13/17 
  Sources   :   https://stackoverflow.com/questions/9119245/haskell-folding-nested-lists
                https://stackoverflow.com/questions/7376937/fastest-way-to-get-the-last-element-of-a-list-in-haskell
                http://hackage.haskell.org/package/base-4.4.0.0/docs/src/GHC-List.html#last
                https://ghc.haskell.org/trac/ghc/wiki/LambdasVsPatternMatching
  Notes     :   The Prelude (++) function is redefined as (+++)
                The Prelude last function is redefined as last'
                The Prelude length function is redefined as length'
                The Prelude tail function is redefined as tail'
                The last function looks butt fuck ugly, but hey it works
  Contents  :   Another weekend wasted on th-... err I mean utilized to learn a such a beautiful language
-}

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = x : ((+++) xs ys)

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last xs
last' [] = error "empty list"

tail' :: [a] -> [a]
tail' (_:xs) =  xs
tail' [] =  error "empty list"

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

{- Takes a Int list and a range that is represent by a Int tuple and returns a Int list of 
   the elements inside the input list within a given range (inclusive). The elements of the 
   returned list must be in the same order they appeared in the input list
   
   listRange [1,3,4,5,2] (1,3) 
   [1,3,2]
-}
listRange :: [Int] -> (Int,Int) -> [Int]
listRange list (start, end) = filter (\x -> if x >= start && x <= end then True else False) list

{- Takes in a list of Int lists and returns the sum of all the numbers inside the list of Int 
   lists 

   maxSum [[1,2,3],[],[5]] 
   11
-}
maxSum :: [[Int]] -> Int
maxSum list = foldl (\accum x -> (foldl (+) 0 x) + accum) 0 list

{- Takes in two arugments: a function that represents a equivalence relation, and a Int list. 
   The function returns a list containing the same elements as the input list, but without any 
   duplicates, where two elements are considered equal if applying the equivalence relation input 
   function to them yields true. This function should only remove consecutive duplicate values 
   from a list based on the equivalence relation function

   dedupe (==) [1, 1, 1, 3, 4, 4, 3]
   [1, 3, 4, 3]
-}

dedupe :: (Int -> Int -> Bool) -> [Int] -> [Int]
dedupe eqFunc list = foldl (\accum x -> if accum == [] || not (eqFunc (last' accum) x) then (+++) accum [x] else accum) [] list

{- Takes in a Int list and returns a list of all non-empty prefixes of a list, ordered from 
   shortest to longest 

   prefixes [1,2,3,4]
   [[1], [1,2], [1,2,3], [1,2,3,4]]
-}
prefixes :: [Int] -> [[Int]]
prefixes list = foldl (\accum x -> if accum == [] then (+++) accum [[x]] else (+++) accum [(+++) (last' accum) [x]]) [] list

{- Takes in a Int list and an Int. The function returns the contiguous sublist of length k whose 
   elements have the largest sum 

   kSublist [1, 2, 4, 5, 8] 3 
   [4, 5, 8]

   The accum in this case is a tuple holding (lastk_1, maxSum)
   - lastk_1 holds the previous 2 elements of the list
   - maxSum holds the sublist of length k whose elements have largest sum
   - we fold over the list and update lastk_1 and maxSum accordingly
   - it would look much cleaner only if I didn't have to do pattern matching and check for other edge cases...
   - WARNING: It does not work with k < 1
-}

kSublist :: [Int] -> Int -> [Int]
kSublist list k = snd (foldl 
  (\ (lastk_1, maxSum) n ->
  if k < 1 then ([], [])
  else if k == 1 then if (sumOfList [n]) < (sumOfList maxSum) then ([], maxSum) else ([], (+++) lastk_1 [n])
  else if ((length' lastk_1) < k - 1) then ((+++) lastk_1 [n], (+++) lastk_1 [n])      
  else if (sumOfList ((+++) lastk_1 [n])) < (sumOfList maxSum) then ((+++) (tail' lastk_1) [n], maxSum) else ((+++) (tail' lastk_1) [n], (+++) lastk_1 [n])
  ) ([], []) list)

sumOfList :: [Int] -> Int
sumOfList list = foldl (+) 0 list







