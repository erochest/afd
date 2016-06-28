{-# LANGUAGE OverloadedStrings #-}


module ArticlesDel.Actions.Init where


import           Control.Error
import qualified Data.Text               as T
import           Database.Persist.Sqlite

import           ArticlesDel.Types


initAction :: T.Text -> Script ()
initAction input = runSqlite input $
    runMigration migrateAll
