module Main where

import qualified Data.ByteString.Lazy as B

import Data.Csv (decode, HasHeader(NoHeader))
import Data.List(intercalate)
import Data.Monoid((<>))
import Data.Vector(Vector)
import qualified Data.Vector as V
import Options.Applicative

import BasicTypes
import Classification
import LineParser
import Parser

data Output = Classification
            | CSV
            | ParsedLines
            | Swimmers
              deriving (Show, Read, Enum)

data Options = Options {
                         fileName :: Maybe FilePath
                         , distanceO :: Distance
                         , sexO :: Sex
                         , years :: [Year]
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
   <*> timeFieldOption
   <*> outputOption

distanceOption = option (Distance <$> auto)
                 (long "distance"
                  <> short 'd'
                  <> metavar "DISTANCE"
                  <> value (Distance 50)
                  <> help "Distance of the races"
                  )

sexOption = flag' Men (long "men" <> short 'm' <> help "Men results")
            <|> flag' Women (long "women" <> short 'w' <> help "Women results")

outputOption = option auto
               (long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> value Classification
                <> help ("Expected output, one of: " ++ intercalate ", " (map show [Classification ..]))
                )

yearOption = some (option (Year <$> auto)
                   (long "year"
                    <> short 'y'
                    <> metavar "YEAR"
                    <> help "Year of the swimmers"
                   )
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

readInput :: Options -> IO (Vector (Vector String))
readInput opts = do
     all <- case fileName opts of
               Nothing -> B.getContents
               Just fn -> B.readFile fn
     let lines = decode NoHeader all :: Either String (Vector (Vector String))
     case lines of
         Right v -> return v
         Left m -> error m

printClassification :: [CLine] -> IO ()
printClassification = mapM_ (putStr . prettyCLine)

process :: Options -> Vector (Vector String) -> IO ()
process opts lines = let
   parsedLines = V.map parseLine lines
   swimmers = parseResults (timeField opts) lines
   d = distanceO opts
   races = [(d, Butterfly), (d, BackStroke), (d, BreastStroke), (d, FreeStyle)]
   classification = classify (sexO opts) (years opts) races swimmers
 in case output opts of
        Classification -> printClassification classification
        CSV -> print lines
        ParsedLines -> print parsedLines
        Swimmers -> print swimmers

main :: IO ()
main = do
         opts <- execParser opt
         lines <- readInput opts
         process opts lines
