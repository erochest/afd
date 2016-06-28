{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}


module ArticlesDel.Types.Internal where


import           Data.Aeson
import           Data.Data
import           Database.Persist.TH
import           GHC.Generics


data ArticleCategory
    = Biography
    | NonBiography
    deriving (Show, Read, Eq, Data, Typeable, Generic)
derivePersistField "ArticleCategory"

instance FromJSON ArticleCategory
instance ToJSON ArticleCategory

data ArticleStatus
    = Draft
    | Published
    | MarkedDeletion
    | Deleted
    deriving (Show, Read, Eq, Data, Typeable, Generic)
derivePersistField "ArticleStatus"

instance FromJSON ArticleStatus
instance ToJSON ArticleStatus

data EditCategory
    = EditAddition
    | EditDeletion
    | EditModification
    deriving (Show, Read, Eq, Data, Typeable, Generic)
derivePersistField "EditCategory"

instance FromJSON EditCategory
instance ToJSON EditCategory

data ActionCategory
    = EditAction
    | CommentAction
    deriving (Show, Read, Eq, Data, Typeable, Generic)
derivePersistField "ActionCategory"

instance FromJSON ActionCategory
instance ToJSON ActionCategory

data ActionStatus
    = KeepAsIsAction
    | KeepModifyAction
    | MergeAction
    | DeleteAction
    | SandboxAction
    deriving (Show, Read, Eq, Data, Typeable, Generic)
derivePersistField "ActionStatus"

instance FromJSON ActionStatus
instance ToJSON ActionStatus
