module BasicTypes (
     Style(..)
   , Name
   , Club
   , Distance(..)
   , Sex(..)
   , Time(..)
   , Year(..)
   , mkTime
   , sumTimes
   , prettyTime
) where

import Data.Monoid ((<>))

data Style = FreeStyle
           | BreastStroke
           | BackStroke
           | Butterfly
           | Medley deriving (Eq, Show)

type Name = String
type Club = String

newtype Distance = Distance Int deriving (Eq, Show)

data Sex = Men | Women | Mixed deriving (Eq, Show)

data Time = NoTime | Time { centiSeconds :: Int, missing :: Int } deriving (Eq, Show)

mkTime :: Int -> Time
mkTime cs = Time cs 0

instance Ord Time where
  compare NoTime NoTime = EQ
  compare NoTime _ = LT
  compare (Time cs1 m1) (Time cs2 m2) = compare m1 m2 <> compare cs1 cs2

sumTimes :: [Time] -> Time
sumTimes = foldr step (Time 0 0)
            where step NoTime (Time t m) = Time t (m+1)
                  step (Time cs1 m1) (Time cs2 m2) = Time (cs1 + cs2) (m1 + m2)

prettyTime :: Time -> String
prettyTime NoTime =  "---"
prettyTime t = let
                 cc = centiSeconds t `mod` 100
                 s = centiSeconds t `div` 100
                 ss = s `mod` 60
                 m = s `div` 60
                 n0 n = if n < 10
                        then '0' : show n
                        else show n
                 pt = if m > 0
                       then show m ++ ":" ++ n0 ss ++ "." ++ n0 cc 
                       else show ss ++ "." ++ n0 cc
               in case missing t of
                     0 -> pt
                     1 -> pt ++ " (falta una)"
                     2 -> pt ++ " (faltan dos)"
                     3 -> pt ++ " (faltan tres)"
                     n -> pt ++ " (faltan " ++ show n ++ ")"

newtype Year = Year Int deriving (Eq, Show)
