module Main where

import Data.Monoid((<>))
import Options.Applicative

import Data.Csv (decode, HasHeader(NoHeader))

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

main :: IO ()
main = do
         opts <- execParser opt
         print opts
