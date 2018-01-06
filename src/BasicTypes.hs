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
   , prettyTime
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

prettyTime :: Time -> String
prettyTime t = let
                 cc = centiSeconds t `mod` 100
                 s = centiSeconds t `div` 100
                 ss = s `mod` 60
                 m = s `div` 60
                 n0 n = if n < 10
                        then '0' : show n
                        else show n
               in if m > 0
                  then show m ++ ":" ++ n0 ss ++ "." ++ n0 cc 
                  else show ss ++ "." ++ n0 cc

newtype Year = Year Int deriving (Eq, Show)
