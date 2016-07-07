{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}


module ArticlesDel.Actions.SampleXml.Types where


import           Control.Foldl     (FoldM)
import           Control.Lens
import           Data.Sequence
import           Data.XML.Types
import           System.Random.MWC


data StateNode
    = Prefix
    | Accum
    | Between
    | Suffix
    deriving (Show, Eq)
$(makePrisms ''StateNode)

type Sampler m a = FoldM m a [a]

data SampleState
    = S
    { _sState       :: !StateNode
    , _sName        :: !Name
    , _sN           :: !Int
    , _sAccum       :: !(Maybe (Seq Event))
    , _sSampleState :: !(Int, GenIO)
    , _sSample      :: !(Sampler IO (Seq Event))
    }
$(makeClassy ''SampleState)
