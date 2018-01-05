module BasicTypes (
     Style(..)
   , Name
   , License
   , Club
   , Distance(..)
   , Sex(..)
   , Time(..)
   , Year(..)
) where

data Style = FreeStyle
           | BreastStroke
           | BackStroke
           | Butterfly
           | Medley deriving Show

type Name = String
type License = String
type Club = String

newtype Distance = Distance Int deriving Show

data Sex = Men | Women | Mixed deriving Show

newtype Time = Time Int deriving Show

newtype Year = Year Int deriving Show
