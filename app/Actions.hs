{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           ArticlesDel.Actions.ImportXml
import           ArticlesDel.Actions.Init
import           ArticlesDel.Actions.SampleXml

import           Types


action :: Actions -> Script ()

action ImportXml{..} = importXml xmlDbFile xmlFile
action Init{..}      = initAction initDbFile
action SampleXml{..} = sampleXml xmlFile sampleN
