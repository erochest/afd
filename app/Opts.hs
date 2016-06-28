{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import qualified Data.Text           as T
import           Options.Applicative

import           Types


txt :: ReadM T.Text
txt = T.pack <$> str

initOpts :: Parser Actions
initOpts = Init <$> option txt (  short 'f' <> long "db-file"
                               <> metavar "FILENAME" <> value "afd.sqlite3"
                               <> help "The file name to initialize a SQLite3\
                                       \ database in.")

opts' :: Parser Actions
opts' = subparser
      (  command "init" (info (helper <*> initOpts)
                          (progDesc "Initialize a database for importing the\
                                    \ data."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Gathering data on Wikipedia Articles for Deletion."
            <> header "afd - Gathering data on Wikipedia Articles for Deletion.")

parseOpts :: IO Actions
parseOpts = execParser opts
