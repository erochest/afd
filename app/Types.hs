module Types where


import qualified Data.Text as T


data Actions
        = Init { initDbFile :: !T.Text
               }
        deriving (Show, Eq)
