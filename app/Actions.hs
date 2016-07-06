{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           ArticlesDel.Actions.ImportXml
import           ArticlesDel.Actions.Init

import           Types


action :: Actions -> Script ()

action Init{..}      = initAction initDbFile
action ImportXml{..} = importXml xmlDbFile xmlFile
