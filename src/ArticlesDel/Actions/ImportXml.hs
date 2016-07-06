{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module ArticlesDel.Actions.ImportXml where


import           Conduit
import           Control.Error
import           Control.Lens                        hiding ((|>))
import           Control.Monad                       (void, when)
import qualified Data.ByteString                     as BS
import           Data.Char                           (isAlphaNum)
import           Data.Foldable
import qualified Data.HashMap.Strict                 as M
import           Data.Monoid
import           Data.Sequence                       ((|>))
import qualified Data.Sequence                       as Seq
import qualified Data.Text                           as T
import           Data.Time
import           Data.XML.Types
import           Database.Persist.Sqlite             hiding (cName)
import           Text.XML.Stream.Parse

import           ArticlesDel.Actions.ImportXml.Types
import           ArticlesDel.Types
import           ArticlesDel.Utils


importXml :: T.Text -> Maybe String -> Script ()
importXml dbFile inputFile = runSqlite dbFile $ do
    runMigration migrateAll
    s <- walkXml inputFile
    if s ^. sState == SMStart
       then transactionSave
       else do
           transactionUndo
           fail $ "Invalid end state: " ++ show (_sState s)

walkXml :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
        => Maybe FilePath -> ImportXmlT m State
walkXml inputFile =
    input =$ parseBytes def $$ foldMC step start
    where
        input :: (MonadThrow m, MonadIO m, MonadBase IO m)
              => Producer (ImportXmlT m) BS.ByteString
        input = maybe stdinC sourceFile inputFile

        start = S SMStart M.empty 0 Nothing Nothing Nothing

cleanChar :: Char -> Char
cleanChar c | isAlphaNum c = c
            | otherwise    = '_'

step :: MonadIO m => State -> Event -> ImportXmlT m State

step s@(S SMStart _ _ _ _ _) (EventBeginElement "page" _) =
    return $ s & sState .~ SMPage
               & sPage .~ Just (PInfo Nothing Nothing Nothing Nothing)
step s@(S SMStart _ _ _ _ _) _ = return s

step s@(S SMPage _ _ _ _ _) (EventEndElement "page") = do
    let page = s ^. sPage
        rev  = s ^. sRevision
    now <- liftIO getCurrentTime
    void . runMaybeT $ do
        pId   <- hoistMaybe $ view pWikiId =<< page
        title <- hoistMaybe $ view pTitle  =<< page
        rId   <- hoistMaybe $ view rWikiId =<< rev
        date  <- hoistMaybe $ view rDate   =<< rev
        let url = "https://en.wikipedia.org/wiki/" <> T.map cleanChar title
        aId   <- lift . insert $ Article pId title url Nothing Nothing Nothing
                                    Nothing Nothing Nothing now Published
                                    Nothing
        void . lift . insert $ Revision rId Nothing date aId Nothing
                                    (view rComment =<< rev)
        when ((s ^. sPageCount + 1) `rem` 10000 == 0) $
           lift transactionSave
    return $ s & sState     .~ SMStart
               & sPage      .~ Nothing
               & sPageCount +~ 1
step s@(S SMPage _ _ _ _ _) (EventBeginElement "id" _) =
    return $ s & sState .~ getId SMPage (sPage . _Just . pWikiId)
step s@(S SMPage _ _ _ _ _) (EventBeginElement "title" _) =
    return $ s & sState .~ SMText "title" Seq.empty SMPage
                            (set (sPage . _Just . pTitle) . Just)
step s@(S SMPage _ _ _ _ _) (EventBeginElement "redirect" _) =
    return $ s & sState .~ SMStart
               & sPage  .~ Nothing
step s@(S SMPage _ _ _ _ _) (EventBeginElement "revision" _) =
    return $ s & sState    .~ SMRevision
               & sRevision .~ Just (RInfo Nothing Nothing Nothing Nothing)
step s@(S SMPage _ _ _ _ _) _ =
    return s

step s@(S SMRevision _ _ _ _ _) (EventEndElement "revision") =
    return $ s & sState .~ SMPage
               & sPage . _Just . pRevision .~ (s ^. sRevision)
               & sRevision .~ Nothing
step s@(S SMRevision _ _ _ _ _) (EventBeginElement "contributor" _) =
    return $ s & sState .~ SMContributor
               & sContributor .~ Just (CInfo Nothing Nothing)
step s@(S SMRevision _ _ _ _ _) (EventBeginElement "id" _) =
    return $ s & sState .~ getId SMRevision (sRevision . _Just . rWikiId)
step s@(S SMRevision _ _ _ _ _) (EventBeginElement "timestamp" _) =
    return $ s & sState .~ SMText "timestamp" Seq.empty SMRevision
                                ( set (sRevision . _Just . rDate)
                                . parseTimeM True defaultTimeLocale "%FT%TZ"
                                . T.unpack
                                )
step s@(S SMRevision _ _ _ _ _) (EventBeginElement "comment" _) =
    return $ s & sState .~ SMText "comment" Seq.empty SMRevision
                                (set (sRevision . _Just . rComment) . Just)
step s@(S SMRevision _ _ _ _ _) _ =
    return s

step s@(S SMContributor _ _ _ _ _) (EventEndElement "contributor") = do
    (userIndex, mCID) <- fmap (fromMaybe (s ^. sUsers, Nothing)) . runMaybeT $ do
        contrib <- hoistMaybe $ s ^. sContributor
        name    <- hoistMaybe $ _cName   contrib
        wid     <- hoistMaybe $ _cWikiId contrib
        case M.lookup name (s ^. sUsers) of
             Just cid -> return (s ^. sUsers, Just cid)
             Nothing  -> do
                 cid <- lift . insert $  Contributor wid name False Nothing
                 return (M.insert name cid $ s ^. sUsers, Just cid)
    return $ s
           & sState .~ SMRevision
           & sUsers .~ userIndex
           & sRevision . _Just . rContrib .~ mCID
step s@(S SMContributor _ _ _ _ _) (EventBeginElement "username" _) =
    return $ s & sState .~ SMText "username" Seq.empty SMContributor
                                (set (sContributor . _Just . cName) . Just)
step s@(S SMContributor _ _ _ _ _) (EventBeginElement "id" _) =
    return $ s & sState .~ getId SMContributor (sContributor . _Just . cWikiId)
step s@(S SMContributor _ _ _ _ _) _ =
    return s

step s@(S (SMText name buffer next f) _ _ _ _ _) (EventEndElement name')
    | name == name' = return . f (fold buffer) $ s & sState .~ next
    | otherwise     = return s
step s@(S SMText{} _ _ _ _ _) (EventContent c) =
    return $ s & over (sState . smtBuffer) (|> getContentText c)
step s@(S SMText{} _ _ _ _ _) _ = return s

ensureHas :: State -> Lens' State (Maybe a) -> a -> State
ensureHas s l x = s & over l (Just . fromMaybe x)

getContentText :: Content -> T.Text
getContentText (ContentText   t) = t
getContentText (ContentEntity t) = "&" <> t <> ";"

getId :: forall s a b. Integral b
      => StateNode s -> ASetter s s a (Maybe b) -> StateNode s
getId returnState l =
    SMText "id" Seq.empty returnState (set l . hush . decimalE)

