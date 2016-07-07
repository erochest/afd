module Types where


import qualified Data.Text as T


data Actions
        = Init { initDbFile :: !T.Text
               }
        | ImportXml { xmlDbFile :: !T.Text
                    , xmlFile   :: !(Maybe FilePath)
                    }
        | SampleXml { sampleN :: !Int
                    , xmlFile :: !(Maybe FilePath)
                    }
        deriving (Show, Eq)
