{-# LANGUAGE TemplateHaskell #-}


module ArticlesDel.Actions.ImportXml.Types where


import           Conduit
import           Control.Error
import           Control.Lens
import           Control.Monad.Logger
import qualified Data.HashMap.Strict     as M
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import           Data.Time
import           Data.XML.Types
import           Database.Persist.Sqlite hiding (cName)

import           ArticlesDel.Types


type ImportXmlT m = SqlPersistT (NoLoggingT (ResourceT (ExceptT String m)))

type UserIndex = M.HashMap T.Text ContributorId

data StateNode s
    = SMStart
    | SMPage
    | SMRevision
    | SMContributor
    | SMText { _smtName   :: !Name
             , _smtBuffer :: !(Seq.Seq T.Text)
             , _smtNext   :: !(StateNode s)
             , _smtSet    :: !(T.Text -> s -> s)
             }
$(makePrisms ''StateNode)
$(makeLenses ''StateNode)

instance Eq (StateNode s) where
    SMStart       == SMStart       = True
    SMPage        == SMPage        = True
    SMRevision    == SMRevision    = True
    SMContributor == SMContributor = True
    SMText{}      == SMText{}      = True
    _             == _             = False

instance Show (StateNode s) where
    show SMStart       = "SMStart"
    show SMPage        = "SMPage"
    show SMRevision    = "SMRevision"
    show SMContributor = "SMContributor"
    show SMText{}      = "SMText{}"

data ContributorInfo
    = CInfo { _cName   :: !(Maybe T.Text)
            , _cWikiId :: !(Maybe Int)
            }
    deriving (Show, Eq)
$(makeClassy ''ContributorInfo)

data RevisionInfo
    = RInfo { _rDate    :: !(Maybe UTCTime)
            , _rContrib :: !(Maybe ContributorId)
            , _rWikiId  :: !(Maybe Int)
            , _rComment :: !(Maybe T.Text)
            }
    deriving (Show, Eq)
$(makeClassy ''RevisionInfo)

data PageInfo
    = PInfo { _pWikiId      :: !(Maybe Int)
            , _pTitle       :: !(Maybe T.Text)
            , _pContributor :: !(Maybe ContributorId)
            , _pRevision    :: !(Maybe RevisionInfo)
            }
    deriving (Show, Eq)
$(makeClassy ''PageInfo)

data State
    = S { _sState       :: !(StateNode State)
        , _sUsers       :: !UserIndex
        , _sPageCount   :: !Int
        , _sPage        :: !(Maybe PageInfo)
        , _sRevision    :: !(Maybe RevisionInfo)
        , _sContributor :: !(Maybe ContributorInfo)
        }
$(makeClassy ''State)

