module Types where


-- import           ArticlesDel.Types


data Actions
        = Default { defaultOutput :: !FilePath
                  , defaultInput  :: !FilePath
                  }
        deriving (Show, Eq)