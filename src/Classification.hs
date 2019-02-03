module Classification (
  CLine
  , classify
  , prettyCLine
  ) where

import Data.List(intercalate, sortOn)

import Swimmer

data CLine = CLine Name Club Year [Time] Time deriving Show

totalTime (CLine _ _ _ _ t) = t

classify :: [(Distance, Style)] -> [Swimmer] -> [CLine]
classify races = sortOn totalTime . map (toCLine races)

toCLine :: [(Distance, Style)] -> Swimmer -> CLine
toCLine races swimmer = let
    rs = results swimmer
    findTime (d, s) = case filter (\r -> distance r == d && style r == s) rs of
                         [] -> NoTime
                         [r] -> time r
    times = map findTime races
 in CLine (name swimmer)
          (club swimmer)
          (year swimmer)
          times
          (sumTimes times)

prettyCLine :: (Int, CLine) -> String
prettyCLine (i, CLine n c (Year y) ts t) = concat [ show i, ". "
                                                  , w 30 n
                                                  , "("
                                                  , c
                                                  , "), "
                                                  , w 5 (show y)
                                                  , "\n\n  * "
                                                  , prettyTime t
                                                  , " ("
                                                  , intercalate ", " (map prettyTime ts)
                                                  , ")\n"
                                                  ]

    where w n p = take (n-1) (p ++ repeat ' ') ++ " "
