module Main where

import Data.Maybe ( fromMaybe )
import Options.Applicative
import System.IO
import Text.Parsec.String ( parseFromFile )

import Lambda.Parser

parseFile = argument' str ( metavar "FILE" )

main :: IO ()
main = do fileName <- execParser optParser
          putStrLn $ "Parsing: " ++ fileName ++ " ..."
          source <- parseFromFile termP fileName 
          putStrLn $ case source of 
            Left parseError -> show parseError
            Right ast -> show ast
  where optParser = info parseFile fullDesc