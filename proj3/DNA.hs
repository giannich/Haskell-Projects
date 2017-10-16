{- 
  File      :   DNA.hs
  Copyright :   (c) Gianni Chen, 10/13/17 
  Sources   :   https://stackoverflow.com/questions/6082090/haskell-deriving-show-for-custom-type
                http://learnyouahaskell.com/making-our-own-types-and-typeclasses
                https://stackoverflow.com/questions/40695023/creating-instance-of-eq-for-custom-data-type-in-haskell
  Notes     :   I'm using the Prelude function "fromEnum" to convert from char to ASCII int
  Contents  :   Definitions for Base, BasePair, Strand and Helix 
-}

data Base = A | C | G | T
data BasePair = BasePair Base Base
data Strand = Strand [Base]
data Helix = Helix [BasePair]

{- 
    Define Show and Eq instances for Base, BasePair, Strand, and Helix. 

    The show for a "Base" value should be the strings: "A" or "T" or "C" or "G"
    The show for a "BasePair" value should be formatted using tuple syntax with the base letter. For example: "(A,T)" or "(G,C)"
    The show for a "Strand" value should be formatted using list syntax with the base letters. For example: "[A,G,C,T]"
    The show for a "Helix" value should be formatted using tuple syntax and list syntax with the base letters. For example: "[(A,G),(C,T)]"

-}

{- For quick testing -}

bs1 = A
bs2 = T

bsPair1 = BasePair A T
bsPair2 = BasePair G C

str1 = Strand [A,G,C,T]
str2 = Strand [T,C,G,A]

hlx1 = Helix [(BasePair A G), (BasePair C T)]
hlx2 = Helix [(BasePair T C), (BasePair G A)]

{- Base Show and Eq implementations, used (==) instead of == to reduce the number of ='s -}
instance Show Base where
    show (A) = "A"
    show (C) = "C"
    show (G) = "G"
    show (T) = "T"

instance Eq Base where
    (==) A A = True
    (==) C C = True
    (==) G G = True
    (==) T T = True
    (==) _ _ = False

{- BasePair Show and Eq implementations -}
instance Show BasePair where
    show (BasePair type1 type2) = show (type1, type2)

instance Eq BasePair where
    (==) (BasePair x1 x2) (BasePair y1 y2) = x1 == y1 && x2 == y2

{- Strand Show and Eq implementations -}
instance Show Strand where
    show (Strand list) = show list

instance Eq Strand where
    (==) (Strand []) (Strand []) = True
    (==) (Strand (x:xs)) (Strand (y:ys)) = x == y && (Strand xs) == (Strand ys)

{- Helix Show and Eq implementations -}
instance Show Helix where
    show (Helix []) = []
    show (Helix pairList) = show pairList

instance Eq Helix where
    (==) (Helix []) (Helix []) = True
    (==) (Helix (x:xs)) (Helix (y:ys)) = x == y && (Helix xs) == (Helix ys)

{-
    wccHelix :: Strand -> Helix. 
    Given a Strand, generate a Helix.

    Requirements 
    -------------
    1. This function must use list comprehension somewhere. You can choose.
    2. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    3. You can define helper functions but they can only use HOFs if they return lists.  

    Hint: This function can be written in one line.

    Example: 
    Main*> wccHelix (Strand [A,T,C,G]) 
    [(A,T),(T,A),(C,G),(G,C)] 
-}

wccHelix :: Strand -> Helix
wccHelix (Strand baseList) = Helix $ [BasePair base (complementBase base) | base <- baseList]

complementBase :: Base -> Base
complementBase base
    | base == A = T
    | base == T = A
    | base == C = G
    | base == G = C
    | otherwise = error "Not a Base"

{- 
    makeHelix :: String -> Helix. 
    Given a String of base letters, make a Helix.

    Requirements 
    -------------
    1. This function must use list comprehension somewhere. You can choose.
    2. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    3. You can define helper functions but they can only use HOFs if they return lists.  
    4. You can reuse functions defined in this file. 


    Hint: This function can be written in one line.


    Example: 
    Main*> makeHelix "ACTG" 
    [(A,T),(C,G),(T,A),(G,C)]
-} 

makeHelix :: String -> Helix
makeHelix string = wccHelix (Strand $ [charToBase char | char <- string])

charToBase :: Char -> Base
charToBase char
    | char == 'A' = A
    | char == 'T' = T
    | char == 'C' = C
    | char == 'G' = G
    | otherwise = error "Not a Base"

{- 
   willAnneal :: Strand -> Strand -> Bool. 
   Determine whether two strands will perfectly anneal (i.e., every base is a Watson-Crick complementarity).
   
    Requirements 
    -------------
    1. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    2. You can define helper functions but they can only use HOFs, if they return lists.  
    3. You can reuse functions defined in this file. 


   Main*> willAnneal (Strand [A,C,T,G]) (Strand [T,G,A,C]) 
   True
   Main*> willAnneal (Strand [A,C,T,T]) (Strand [T,G,A,C])
   False
-} 

willAnneal :: Strand -> Strand -> Bool
willAnneal (Strand baseList) strand = (Strand $ map complementBase baseList) == strand