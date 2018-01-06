module Classification (
  CLine
  , classify
  ) where

import Data.List(sortOn)

import Swimmer

data CLine = CLine License Name Club Year [Time] Time deriving Show

totalTime (CLine _ _ _ _ _ t) = t

classify :: Sex -> [Year] -> [(Distance, Style)] -> Swimmers -> [CLine]
classify sexw years races swimmers = let
   selected = selectSwimmers (\sw -> sex sw == sexw && year sw `elem` years) swimmers
  in sortOn totalTime $ map (toCLine races) selected

toCLine :: [(Distance, Style)] -> Swimmer -> CLine
toCLine races swimmer = let
    rs = results swimmer
    findTime (d, s) = case filter (\r -> distance r == d && style r == s) rs of
                         [] -> Time 300000
                         [r] -> time r
    times = map findTime races
 in CLine (license swimmer)
          (name swimmer)
          (club swimmer)
          (year swimmer)
          times
          (sumTimes times)
              
