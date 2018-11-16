module Lib
    ( parseJournal
    , txjoin
    , showTransactions
    ) where

import Data.List (intercalate, partition, find)
import GHC.Exts (sortWith)
import Hledger.Data.Transaction (showTransaction)
import Hledger.Data.Types (Transaction, tdate, tpostings, tdescription, Posting, pamount, Journal, jtxns, MixedAmount)
import Hledger.Query (Query(..), matchesTransaction, matchesPosting)
import Hledger.Read (readJournal)
import Hledger.Read.Common (definputopts)
import qualified Data.Text as Text

parseJournal :: IO (Either String Journal)
parseJournal =
    getContents >>= readJournal definputopts Nothing . Text.pack

qUnknown :: Query
qUnknown = Acct "unknown"

txjoin :: Journal -> [Transaction]
txjoin input =
    let
        txs = jtxns input
        (unknown, known) = partition (matchesTransaction qUnknown) txs
        matched = findMatches unknown
    in
        sortWith tdate (known ++ matched)

findMatches :: [Transaction] -> [Transaction]
findMatches [] = []
findMatches [tx] = [tx]
findMatches (tx:txs) =
    let
        (tx', txs') = matchTx tx txs
    in
        tx' : findMatches txs'

matchTx :: Transaction -> [Transaction] -> (Transaction, [Transaction])
matchTx tx txs =
    let
        isSameDay tx' = tdate tx' == tdate tx
        isSameDescr tx' = tdescription tx' == tdescription tx
        isRightAmount tx' = (unknownAmount tx' == knownAmount tx)
        predicate tx' = all ($ tx') [isSameDay, isSameDescr, isRightAmount]
        result = find predicate txs
    in
        case result of
            Nothing -> (tx, txs)

            Just tx' -> (mergeTx tx tx', filter (not . predicate) txs)

getAmount :: (Posting -> Bool) -> Transaction -> Maybe MixedAmount
getAmount predicate =
    fmap pamount . find predicate . tpostings

unknownAmount :: Transaction -> Maybe MixedAmount
unknownAmount = getAmount (matchesPosting qUnknown)

knownAmount :: Transaction -> Maybe MixedAmount
knownAmount = getAmount (not . matchesPosting qUnknown)

mergeTx :: Transaction -> Transaction -> Transaction
mergeTx tx1 tx2 = 
    let
        allPostings = tpostings tx1 ++ tpostings tx2
        knownPostings = filter (not . matchesPosting qUnknown) allPostings 
    in
        tx1 { tpostings = knownPostings } 

showTransactions :: [Transaction] -> String
showTransactions = 
        intercalate "\n" . map showTransaction 