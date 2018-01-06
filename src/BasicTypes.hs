module BasicTypes (
     Style(..)
   , Name
   , License
   , Club
   , Distance(..)
   , Sex(..)
   , Time(..)
   , Year(..)
   , sumTimes
) where

data Style = FreeStyle
           | BreastStroke
           | BackStroke
           | Butterfly
           | Medley deriving (Eq, Show)

type Name = String
type License = String
type Club = String

newtype Distance = Distance Int deriving (Eq, Show)

data Sex = Men | Women | Mixed deriving (Eq, Show)

newtype Time = Time { centiSeconds :: Int } deriving (Eq, Ord, Show)

sumTimes :: [Time] -> Time
sumTimes = Time . sum . map centiSeconds

newtype Year = Year Int deriving (Eq, Show)
