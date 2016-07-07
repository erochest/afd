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

dbFileOpt :: Parser T.Text
dbFileOpt =
    option txt (  short 'f' <> long "db-file"
               <> metavar "FILENAME" <> value "afd.sqlite3"
               <> help "The file name to initialize a SQLite3\
                       \ database in.")

initOpts :: Parser Actions
initOpts = Init <$> dbFileOpt

importXmlOpts :: Parser Actions
importXmlOpts =
    ImportXml
        <$> dbFileOpt
        <*> optional (strOption (  short 'i' <> long "input-file"
                                <> metavar "INPUT_FILE"
                                <> help "The input XML file. Omitting this\
                                        \ will read from STDIN."))

sampleXmlOpts :: Parser Actions
sampleXmlOpts =
    SampleXml
        <$> option auto (  short 'n' <> long "n" <> metavar "N" <> value 1000
                        <> help "The number of pages to include in the sample.\
                                \ Default is 1000.")
        <*> optional (strOption (  short 'i' <> long "input-file"
                                <> metavar "INPUT_FILE"
                                <> help "The input XML file. Omitting this\
                                        \ will read from STDIN."))

opts' :: Parser Actions
opts' = subparser
      (  command "init" (info (helper <*> initOpts)
                          (progDesc "Initialize a database for importing the\
                                    \ data."))
      <> command "import-xml" (info (helper <*> importXmlOpts)
                                (progDesc "Import a database dump XML file\
                                          \ into the database."))
      <> command "sample-xml" (info (helper <*> sampleXmlOpts)
                                (progDesc "Sample a database dump XML file\
                                          \ for testing."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Gathering data on Wikipedia Articles for Deletion."
            <> header "afd - Gathering data on Wikipedia Articles for Deletion.")

parseOpts :: IO Actions
parseOpts = execParser opts
