module LineParser (
  LineInfo(..)
  , TimeLineData
  , isTime
  , getTime
  , parseLine
) where

import Control.Monad(mzero, unless, when)
import Data.Char(digitToInt, isDigit, isSpace)
import Data.Maybe(fromMaybe)
import Data.Vector(Vector, (!))
import qualified Data.Vector as V
import Data.Void(Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import BasicTypes

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

parseLine :: Vector String -> LineInfo
parseLine v = fromMaybe OtherLine
            ( trySwimmerLine v
            <|> tryRaceLine v
            <|> tryTimeLine v
            )

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
sexParser = stringParser [("MASCULINO", Men), ("FEMENINO", Women), ("MIXTO", Mixed),
                          ("Masculino", Men), ("Femenino", Women), ("Mixto", Mixed)]

natParser :: Parser Int
natParser = read <$> some digitChar

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
    return $ Time n


