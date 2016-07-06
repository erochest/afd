module ArticlesDel.Utils where


import qualified Data.Text      as T
import           Data.Text.Read


decimalE :: Integral a => T.Text -> Either String a
decimalE x = case decimal x of
                  Right (a, t) | T.null t  -> Right a
                               | otherwise -> Left "Left over text."
                  Left e -> Left e
