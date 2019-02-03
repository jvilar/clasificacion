module Main where

import qualified Data.ByteString.Lazy as B

import Data.Csv (decode, HasHeader(NoHeader))
import Data.List(intercalate)
import Data.Monoid((<>))
import Data.Vector(Vector)
import qualified Data.Vector as V
import Options.Applicative
import Text.Show.Pretty(pPrint)

import BasicTypes
import Classification
import LineParser
import Parser
import Swimmer

data Output = Classification
            | Header
            | ParsedLines
            | Swimmers
              deriving (Show, Read, Enum)

data Options = Options {
                         fileName :: Maybe FilePath
                         , distanceO :: Distance
                         , sexO :: Maybe Sex
                         , years :: [Year]
                         , clubO :: Maybe Club
                         , timeField :: Int
                         , output :: Output
                       } deriving Show

opt = info (optParser <**> helper)
       (fullDesc
       <> progDesc "Rank swimmers by their marks"
       <> header "clasificacion")

optParser :: Parser Options
optParser = Options
   <$> fileArgument
   <*> distanceOption
   <*> sexOption
   <*> yearOption
   <*> clubOption
   <*> timeFieldOption
   <*> outputOption

distanceOption = option (Distance <$> auto)
                 (long "distance"
                  <> short 'd'
                  <> metavar "DISTANCE"
                  <> value (Distance 50)
                  <> help "Distance of the races"
                  )

sexOption = flag' (Just Men) (long "men" <> short 'm' <> help "Men results")
            <|> flag' (Just Women) (long "women" <> short 'w' <> help "Women results")
            <|> pure Nothing

outputOption = option auto
               (long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> value Classification
                <> help ("Expected output, one of: " ++ intercalate ", " (map show [Classification ..]))
                )

yearOption = many (option (Year <$> auto)
                   (long "year"
                    <> short 'y'
                    <> metavar "YEAR"
                    <> help "Year of the swimmers"
                   )
                  )

clubOption = option (Just <$> str)
                   (long "club"
                    <> short 'c'
                    <> metavar "CLUB"
                    <> value Nothing
                    <> help "Club of the swimmers"
                   )

timeFieldOption = option auto
                 (long "timeField"
                  <> short 't'
                  <> metavar "INT"
                  <> value 3
                  <> help "Position in the line where the time is found"
                  )

helpOption = switch ( long "help"
                    <> short 'h'
                    <> help "this help")

fileArgument = (Just <$> argument str (metavar "FILE"))
               <|> pure Nothing

readInput :: Options -> IO [String]
readInput opts = lines <$> case fileName opts of
                             Nothing -> getContents
                             Just fn -> readFile fn

printClassification :: [CLine] -> IO ()
printClassification = mapM_ (putStr . prettyCLine) . zip [1..]

process :: Options -> [String] -> IO ()
process opts lines = let
   header = parseHeaderLine $ head lines
   parsedLines = map (parseLine header) $ tail lines
   swimmers = parseResults lines
   filter = let
               sf = case sexO opts of
                          Nothing -> const True
                          Just s -> (== s) . sex
               yf = case years opts of
                      [] -> const True
                      l -> (`elem` l) . year
               cf = case clubO opts of
                      Nothing -> const True
                      Just c -> (== c) . club
            in \sw -> sf sw && yf sw && cf sw
   selected = selectSwimmers filter swimmers
   d = distanceO opts
   races = [(d, Butterfly), (d, BackStroke), (d, BreastStroke), (d, FreeStyle)]
   classification = classify races selected
 in case output opts of
        Classification -> printClassification classification
        Header -> pPrint header
        ParsedLines -> pPrint parsedLines
        Swimmers -> pPrint swimmers

main :: IO ()
main = do
         opts <- execParser opt
         lines <- readInput opts
         process opts lines
