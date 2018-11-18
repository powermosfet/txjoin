module CliOpts where 

import Options.Applicative
import Data.Monoid ((<>))

newtype CliOptions = CliOptions
    { description :: String
    }

options :: Parser CliOptions
options = CliOptions
    <$> strArgument
            ( metavar "DESCR"
              <> help "only match transactions that match the regex DESCR"
            )

optionsWithInfo :: ParserInfo CliOptions
optionsWithInfo = info (options <**> helper)
      ( fullDesc <> progDesc "match transactions between accounts" )
