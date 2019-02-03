module Parser (
  parseResults
) where

import Control.Monad.State
import Data.Vector(Vector)
import qualified Data.Vector as V

import LineParser
import Swimmer

parseResults :: [String] -> Swimmers
parseResults (h:r) = let
  header = parseHeaderLine h
 in foldr (addSwimmer . parseLine header) emptySwimmers r


