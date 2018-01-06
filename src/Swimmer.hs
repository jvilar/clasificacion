module Swimmer (
  Swimmer(..)
  , createSwimmer
  , Result(..)
  , createResult
  , Swimmers
  , emptySwimmers
  , addSwimmer
  , addResult
  , module BasicTypes
) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import BasicTypes

data Swimmer = Swimmer { name :: Name
                       , license :: License
                       , club :: Club
                       , sex :: Sex
                       , year :: Year
                       , results :: [ Result ]
                       } deriving Show

createSwimmer :: Name -> License -> Club -> Sex -> Year -> Swimmer
createSwimmer n l c s y = Swimmer n l c s y []

data Result = Result { distance :: Distance
                     , style :: Style
                     , time :: Time
                     } deriving Show

createResult :: Distance -> Style -> Time -> Result
createResult = Result

type Swimmers = Map License Swimmer

emptySwimmers :: Swimmers
emptySwimmers = M.empty

addSwimmer :: Swimmer -> Swimmers -> Swimmers
addSwimmer s ss = if license s `M.member` ss
                  then ss
                  else M.insert (license s) s ss

addResult :: License -> Result -> Swimmers -> Swimmers
addResult l r ss = let
   s = ss M.! l
   s' = s { results = r : results s }
 in M.insert l s' ss

