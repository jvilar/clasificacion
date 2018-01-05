module Main where

import qualified Data.ByteString.Lazy as B

import Data.Monoid((<>))
import Data.Vector(Vector)
import qualified Data.Vector as V
import Options.Applicative

import Data.Csv (decode, HasHeader(NoHeader))
import Parser

data Options = Options {
                         fileName :: Maybe FilePath
                       } deriving Show

optParser :: Parser Options
optParser = Options
   <$> fileArgument

opt = info ( optParser <**> helper)
       (fullDesc
       <> progDesc "Rank swimmers by their marks"
       <> header "clasificacion")

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

main :: IO ()
main = do
         opts <- execParser opt
         lines <- readInput opts
         let plines = V.map parseLine lines

         print plines
