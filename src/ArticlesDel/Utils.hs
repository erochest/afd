{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module ArticlesDel.Utils where


import           Conduit
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import           Data.Text.Read
import           Data.XML.Types
import           Text.XML.Stream.Parse


decimalE :: Integral a => T.Text -> Either String a
decimalE x = case decimal x of
                  Right (a, t) | T.null t  -> Right a
                               | otherwise -> Left "Left over text."
                  Left e -> Left e

xmlInput :: MonadResource m => Maybe FilePath -> Producer m Event
xmlInput inputFile = input =$ parseBytes def
    where
        input :: MonadResource m => Producer m BS.ByteString
        input = maybe stdinC sourceFile inputFile
