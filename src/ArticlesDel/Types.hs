{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module ArticlesDel.Types
    ( migrateAll

    , Editathon(..)
    , EditathonId
    , Article(..)
    , ArticleId
    , EditathonArticle(..)
    , EditathonArticleId
    , Edit(..)
    , EditId
    , EditorAction(..)
    , EditorActionId
    , WikiEditor(..)
    , WikiEditorId
    , EditathonParticipant(..)
    , EditathonParticipantId
    , Discussion(..)
    , DiscussionId

    , module X
    ) where


import           Data.Data
import           Data.Text                  (Text)
import           Data.Time
import           Database.Persist.Quasi
import           Database.Persist.TH
import           GHC.Generics

import           ArticlesDel.Types.Internal

import qualified ArticlesDel.Types.Internal as X


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
