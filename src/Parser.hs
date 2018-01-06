module Parser (
  parseResults
) where

import Control.Monad.State
import Data.Vector(Vector)
import qualified Data.Vector as V

import LineParser
import Swimmer

parseResults :: Vector(Vector String) -> Swimmers
parseResults v = let
  lines = V.foldr ((:) . parseLine) [] v
 in fsa lines

data FSAState = Initial
                | InRace
                | AfterSwimmer
                | Inserted
                | Error String

data Info = Info { raceI :: Maybe (Distance, Style, Sex)
                 , licenseI :: Maybe License
                 , swimmersI :: Swimmers
                 }

emptyInfo :: Info
emptyInfo = Info Nothing Nothing emptySwimmers

fsa :: [LineInfo] -> Swimmers
fsa ls = swimmersI $ execState (runFSA Initial ls) emptyInfo

type FSA = State Info

runFSA :: FSAState -> [LineInfo] -> FSA ()
runFSA _ [] = return ()
runFSA s (l:ls) = do
   s' <- transition s l
   case s' of
       Error m -> error $ "Parsing error, message " ++ m ++ "\nIn line: " ++ show l
       _ -> runFSA s' ls

transition :: FSAState -> LineInfo -> FSA FSAState
transition Initial SwimmerLine {} = return $ Error "Swimmer outside race"
transition Initial r@RaceLine {} = foundRace r
transition Initial _ = return Initial

transition InRace s@SwimmerLine {} = foundSwimmer s
transition InRace r@RaceLine {} = foundRace r
transition InRace _ = return InRace

transition AfterSwimmer s@SwimmerLine {} = foundSwimmer s
transition AfterSwimmer r@RaceLine {} = foundRace r
transition AfterSwimmer (FirstLegLine _ _ t) = do
    info <- get
    let Just (d, s, _) = raceI info
        Just l = licenseI info
        r = createResult d s t
    put $ info { swimmersI = addResult l r (swimmersI info) }
    return Inserted
transition AfterSwimmer _ = return AfterSwimmer

transition Inserted s@SwimmerLine {} = foundSwimmer s
transition Inserted r@RaceLine {} = foundRace r
transition Inserted _ = return Inserted

foundRace :: LineInfo -> FSA FSAState
foundRace (RaceLine d st s) = do
    info <- get
    put $ info { raceI = Just (d, st, s)
               , licenseI = Nothing
               }
    return InRace

foundSwimmer :: LineInfo -> FSA FSAState
foundSwimmer (SwimmerLine _ l n y c) = do
    info <- get
    let Just (_, _, s) = raceI info
        sw = createSwimmer n l c s y
    put $ info { licenseI = Just l
               , swimmersI = addSwimmer sw (swimmersI info)
               }
    return AfterSwimmer


