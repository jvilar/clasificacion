{-# LANGUAGE LambdaCase #-}
module LineParser (
  parseHeaderLine,
  parseLine
) where

import Control.Monad(forM, mzero, unless, when)
import Control.Monad.State.Strict(get, evalState, execState, runState, State)
import Data.Char(digitToInt, isDigit, isSpace)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(fromJust, isJust, fromMaybe)
import Data.List(takeWhile)
import Data.List.Split(splitOn)
import Data.Vector(Vector, (!))
import qualified Data.Vector as V
import Data.Void(Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BasicTypes
import Swimmer

import Debug.Trace

data HeaderInfo = NameH
                  | BirthDateH
                  | SexH
                  | LongClubH
                  | ShortClubH
                  | ClubNameH
                  | EventNoH Int
                  | StyleH Int
                  | TimeH Int
                  | PlaceH Int
                  | PointH Int
                  | ProgressionH Int
                  | EntryTimeH Int
                  | MkTitleH Int
                  | MkTotalH Int
                  | MkPlaceH Int
                  | UnknownH String
                    deriving (Eq, Ord, Show)

type HeaderLine = [ HeaderInfo ]

parseHeaderLine :: String -> HeaderMap
parseHeaderLine  l = let
  fields = (\case Nothing -> error "Bad header"
                  Just x -> x) $ parseMaybe (tabFields headerParser <* space) l
  step (f, n) m = M.insert f n m
  in foldr step M.empty $ zip fields [0..]

tabFields :: Parser a -> Parser [a]
tabFields = flip sepBy $ char '\t'

headerParser :: Parser HeaderInfo
headerParser = do
  (w, mn) <- wordNumberParser
  let n = fromMaybe 0 mn
  return $ case w of
    "NAME" -> NameH
    "BIRTHDATE" -> BirthDateH
    "GENDER" -> SexH
    "LONGCLUB" -> LongClubH
    "SHORTCLUB" -> ShortClubH
    "CLUBNAME" -> ClubNameH
    "EVENTNO" -> EventNoH n
    "STYLE" -> StyleH n
    "TIME" -> TimeH n
    "PLACE" -> PlaceH n
    "POINT" -> PointH n
    "PROGRESSION" -> ProgressionH n
    "ENTRYTIME" -> EntryTimeH n
    "MKTITLE" -> MkTitleH n
    "MKTOTAL" -> MkTotalH n
    "MKPLACE" -> MkPlaceH n
    _ -> UnknownH $ case mn of
                      Nothing -> w
                      Just n -> w ++ show n


wordNumberParser :: Parser (String, Maybe Int)
wordNumberParser = (,) <$> some letterChar
                       <*> (do
                               n <- many digitChar
                               return $ if null n
                                 then Nothing
                                 else Just $ read n
                           )

type HeaderMap = Map HeaderInfo Int
type FieldsMap = Map Int String
type LineProcessor = State (HeaderMap, FieldsMap)


parseLine :: HeaderMap -> String -> Swimmer
parseLine hmap l = let
  fields = foldr step M.empty . zip [0.. ] $ splitOn "\t" l
  step (n, s) = M.insert n s
  in evalState buildSwimmer (hmap, fields)

buildSwimmer :: LineProcessor Swimmer
buildSwimmer = do
  Just name <- parseP NameH swimmerNameParser
  Just club <- parseP ClubNameH clubParser
  Just sex <- parseP SexH sexParser
  Just year <- parseP BirthDateH yearParser
  results <- getResults 1
  return $ Swimmer { name = name
                   , club = club
                   , sex = sex
                   , year = year
                   , results = results
                   }

getResults :: Int -> LineProcessor [Result]
getResults n = getResult n >>= \case
    Nothing -> return []
    Just r -> (r:) <$> getResults (n+1)

getResult :: Int -> LineProcessor (Maybe Result)
getResult n = do
  ds <- parseP (StyleH n) ( (,) <$>  allowSpace distanceParser <*> styleParser)
  time <- parseP (TimeH n) timeParser
  return $ Result <$> (fst <$> ds) <*> (snd <$> ds) <*> time


parseP :: HeaderInfo -> Parser a -> LineProcessor (Maybe a)
parseP h p = do
  (hmap, fmap) <- get
  return . parseMaybe p $ fmap M.! (hmap M.! h)

data TimeLineData = TLD Distance
                  | TLT Time
                  | TLEmpty deriving Show

isTime :: TimeLineData -> Bool
isTime (TLT _) = True
isTime _ = False

getTime :: TimeLineData -> Time
getTime (TLT t) = t

data LineInfo = SwimmerLine Int License Name Year Club
              | RaceLine Distance Style Sex
              | TimeLine [TimeLineData]
              | OtherLine
              deriving Show


trySwimmerLine :: Vector String -> Maybe LineInfo
trySwimmerLine v = do
    n <- parseMaybe natParser (v ! 0)
    l <- parseMaybe licenseParser (v ! 1)
    name <- parseMaybe swimmerNameParser (v ! 2)
    y <- Year <$> parseMaybe natParser (v ! 3)
    club <- parseMaybe clubParser (v ! 4)
    return $ SwimmerLine n l name y club

tryRaceLine :: Vector String -> Maybe LineInfo
tryRaceLine v = parseMaybe raceLineInfoParser (v ! 0)

tryTimeLine :: Vector String -> Maybe LineInfo
tryTimeLine v = do
                 tls <- V.foldM (\l e -> do
                         x <- (TLT <$> parseMaybe timeParser e)
                              <|> (TLD . Distance <$> parseMaybe natParser e)
                              <|> (if null e
                                      then return TLEmpty
                                      else Nothing
                                  )
                         return $ l ++ [x]) [] v
                 if any isTime tls
                     then return $ TimeLine tls
                     else Nothing

raceLineInfoParser :: Parser LineInfo
raceLineInfoParser = do
    natParser *> space *> char '-' *> space
    d <- distanceParser
    space
    s <- styleParser
    space
    sex <- sexParser
    return $ RaceLine d s sex

type Parser = Parsec Void String

allowSpace :: Parser a -> Parser a
allowSpace = (<* space)

noSpace :: Parser Char
noSpace = satisfy (not . isSpace) <?> "no space"

stringParser :: [(String, a)] -> Parser a
stringParser ls = foldr (<|>) mzero [ try (string s >> return r) | (s, r) <- ls ]

styleParser :: Parser Style
styleParser = stringParser [("LIBRE", FreeStyle), ("ESPALDA", BackStroke),
                            ("BRAZA", BreastStroke), ("MARIPOSA", Butterfly),
                            ("ESTILOS", Medley),
                            ("Libre", FreeStyle), ("Espalda", BackStroke),
                            ("Braza", BreastStroke), ("Mariposa", Butterfly),
                            ("Estilos", Medley)
                           ]

sexParser :: Parser Sex
sexParser = stringParser [("M", Men), ("F", Women)]

natParser :: Parser Int
natParser = read <$> some digitChar

yearParser :: Parser Year
yearParser = Year <$> (natParser *> char '/' *> natParser *> char '/' *> natParser)

distanceParser :: Parser Distance
distanceParser = Distance <$> allowSpace natParser <* char 'm' <* optional (char '.')

licenseParser :: Parser License
licenseParser = do
                  l <- some noSpace
                  unless (any isDigit l) . fail $ "Bad license: " ++ l
                  return l

swimmerNameParser :: Parser Name
swimmerNameParser = unwords <$> sepBy1 (some noSpace) space

clubParser :: Parser Club
clubParser = unwords <$> sepBy1 (some noSpace) space

timeParser :: Parser Time
timeParser = do
    p <- natParser
    n <- ( (\s c -> (p * 60 + s) * 100 + c) <$> (char ':' *> natParser) <* char '.' <*> natParser)
           <|> ( (p*100+) <$> (char '.' *> natParser))
    return $ mkTime n


