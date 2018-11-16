module Main where

import Lib

main :: IO ()
main = do
    journal <- parseJournal
    case journal of
        Left err -> putStrLn err

        Right theJournal -> putStrLn $ showTransactions $ txjoin theJournal
