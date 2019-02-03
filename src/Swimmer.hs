module Swimmer (
  Swimmer(..)
  , createSwimmer
  , Result(..)
  , createResult
  , Swimmers
  , emptySwimmers
  , addSwimmer
  , selectSwimmers
  , module BasicTypes
) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import BasicTypes

data Swimmer = Swimmer { name :: Name
                       , club :: Club
                       , sex :: Sex
                       , year :: Year
                       , results :: [ Result ]
                       } deriving Show

createSwimmer :: Name -> Club -> Sex -> Year -> Swimmer
createSwimmer n c s y = Swimmer n c s y []

data Result = Result { distance :: Distance
                     , style :: Style
                     , time :: Time
                     } deriving Show

createResult :: Distance -> Style -> Time -> Result
createResult = Result

type Swimmers = Map Name Swimmer

emptySwimmers :: Swimmers
emptySwimmers = M.empty

addSwimmer :: Swimmer -> Swimmers -> Swimmers
addSwimmer s ss = if name s `M.member` ss
                  then ss
                  else M.insert (name s) s ss

selectSwimmers :: (Swimmer -> Bool) -> Swimmers -> [ Swimmer ]
selectSwimmers pred = M.foldr step []
   where step s l | pred s = s : l
                  | otherwise = l

