module DataTypes 
(
    SimSettings(..),
    ArgsPackage(..),
    SimState(..),
    City(..),
    Row(..),
    Neighborhood(..),
    House(..),
    Location(..),
    State(..),
    Satisfaction(..),
    OpenHouse(..)
) where 

import Data.Foldable

{- Package of arguments and simstate -}
data SimSettings = SimSettings {argsPack :: ArgsPackage,
                                simState :: SimState,
                                roundNum :: Int,
                                stopped :: Bool,
                                stepDelay :: Float}

{- Package of arguments -}
data ArgsPackage = ArgsPackage {maxRounds :: Int,
                                rows :: Int,
                                cols :: Int,
                                neighRadius :: Int,
                                threshold :: Float,
                                emptyPer :: Float,
                                parksPer :: Float,
                                bluePer :: Float,
                                redPer :: Float}

{- Describes the simulation state -}
data SimState = SimState {city :: City (Row House),
                          openHouse :: OpenHouse Location,
                          rad :: Int,
                          thresh :: Float,
                          elaspedTime :: Float} deriving (Eq)

{- Describes a city -}
data City a = City [a] deriving (Eq)

{- Describes a row -}
data Row a = Row {rowNum :: Int,
                  houseList :: [a]} deriving (Eq)

{- Describes a neighborhood -}
data Neighborhood a = Neighborhood [a] deriving (Eq, Show)

{- Describes a house as a set of location and state -}
data House = House {location :: Location,
                    state :: State} deriving (Eq)

{- Describes a location as a tuple of coordinates -}
type Location = (Int, Int)

{- Describes the state of the household -}
data State = B Satisfaction | R Satisfaction | O | P deriving (Eq)

{- Describes the satisfaction of the household -}
data Satisfaction = Satisfied | Unsatisfied deriving (Eq, Show)

{- Describes the list of open houses -}
data OpenHouse a = OpenHouse [a] deriving (Eq)

-- SimState

instance Show SimState where
  show simState = "\nCity:" ++ show (city simState) ++ 
                  "\nOpen Houses:" ++ show (openHouse simState) ++ 
                  "\nNeighbor Size: " ++ show (rad simState) ++ 
                  "\nSimilarity Threshold: " ++ show (thresh simState) ++ "\n"

-- City

instance (Show a) => Show (City a) where
  show (City []) = "\n"
  show (City (x:xs)) = "\n[" ++ show x ++ "]" ++ show (City xs)

instance Functor City where
  fmap f (City []) = City []
  fmap f (City a) = City (fmap f a)

instance Applicative City where
  pure = (City . pure [])
  (<*>) (City [f]) (City [a]) = (City ((<*>) [f] [a]))

instance Monad City where
  (>>=) (City [a]) f = f a

instance Monoid (City a) where
  mempty = City []
  mappend (City listA) (City listB) = City (mappend listA listB)

instance Foldable City where
  foldMap f (City []) = mempty
  foldMap f (City list) = foldMap f list

-- Row

instance (Show a) => Show (Row a) where
  show row = foldl (\str x -> str ++ (show x) ++ "\t") "[\t" (houseList row) ++ "]"

instance Functor Row where
  fmap f (Row rowNum []) = Row rowNum []
  fmap f (Row rowNum a) = Row rowNum (fmap f a)

instance Applicative Row where
  pure = (Row 0 . pure [])
  (<*>) (Row _ [f]) (Row rowNum [a]) = (Row rowNum ((<*>) [f] [a]))

instance Monad Row where
  (>>=) (Row rowNum [a]) f = f a

instance Monoid (Row a) where
  mempty = Row 0 []
  mappend (Row rowNum listA) (Row _ listB) = Row rowNum (mappend listA listB)

instance Foldable Row where
  foldMap f (Row _ []) = mempty
  foldMap f (Row rowNum list) = foldMap f list

-- Neighborhood

instance Functor Neighborhood where
  fmap f (Neighborhood []) = Neighborhood []
  fmap f (Neighborhood a) = Neighborhood (fmap f a)

instance Applicative Neighborhood where
  pure = (Neighborhood . pure [])
  (<*>) (Neighborhood [f]) (Neighborhood [a]) = (Neighborhood ((<*>) [f] [a]))

instance Monad Neighborhood where
  (>>=) (Neighborhood [a]) f = f a

instance Monoid (Neighborhood a) where
  mempty = Neighborhood []
  mappend (Neighborhood listA) (Neighborhood listB) = Neighborhood (mappend listA listB)

instance Foldable Neighborhood where
  foldMap f (Neighborhood []) = mempty
  foldMap f (Neighborhood list) = foldMap f list

-- House

instance Show House where
  show house = show (location house) ++ ", " ++ show (state house)

-- State

instance Show State where
  show (B Satisfied) = "B S"
  show (B Unsatisfied) = "B U"
  show (R Satisfied) = "R S"
  show (R Unsatisfied) = "R U"
  show O = "O" 
  show P = "P"

-- OpenHouse

instance (Show a) => Show (OpenHouse a) where
  show (OpenHouse []) = "\n"
  show (OpenHouse (x:xs)) = "\n" ++ show x ++ show (OpenHouse xs)

instance Functor OpenHouse where
  fmap f (OpenHouse []) = OpenHouse []
  fmap f (OpenHouse a) = OpenHouse (fmap f a)

instance Applicative OpenHouse where
  pure = (OpenHouse . pure [])
  (<*>) (OpenHouse [f]) (OpenHouse [a]) = (OpenHouse ((<*>) [f] [a]))

instance Monad OpenHouse where
  (>>=) (OpenHouse [a]) f = f a

instance Monoid (OpenHouse a) where
  mempty = OpenHouse []
  mappend (OpenHouse listA) (OpenHouse listB) = OpenHouse (mappend listA listB)

instance Foldable OpenHouse where
  foldMap f (OpenHouse []) = mempty
  foldMap f (OpenHouse list) = foldMap f list

