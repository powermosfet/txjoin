module Main where

import Options.Applicative (execParser)

import Lib
import CliOpts (optionsWithInfo)

main :: IO ()
main = do
    opts <- execParser optionsWithInfo
    transactions <- parseJournal
    case transactions of
        Left err -> putStrLn err

        Right theTransactions -> putStrLn $ showTransactions $ txjoin opts theTransactions
