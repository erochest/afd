{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module ArticlesDel.Actions.SampleXml where


import           Conduit
import           Control.Error
import qualified Control.Foldl                       as F
import           Control.Lens
import           Control.Monad.Primitive
import           Data.Foldable
import qualified Data.IntMap                         as M
import           Data.Sequence                       hiding ((|>))
import qualified Data.Sequence                       as Seq
import           Data.XML.Types
import           System.Random.MWC
import           Text.XML.Stream.Render

import           ArticlesDel.Actions.SampleXml.Types
import           ArticlesDel.Utils


sampleXml :: Maybe FilePath -> Int -> Script ()
sampleXml inputFile k = lift . withSystemRandom . asGenIO $ \gen -> do
    let state = S Prefix "page" 0 Nothing (k, gen) $ sampleInit k gen
    runResourceT $
        xmlInput inputFile
            =$= sampleEvents state
            $$  renderBytes def
            =$= stdoutC

sampleEvents :: MonadIO m => SampleState -> Conduit Event m Event
sampleEvents = concatMapAccumMC step'
    where
        step' e s = liftIO $ step e s

-- The state machine

step :: Event -> SampleState -> IO (SampleState, [Event])

step e@(EventBeginElement en _) s@(S Prefix sn _ _ _ _)
    | en == sn  = toAccum <$> startTag e s
    | otherwise = return (s, [e])
step e s@(S Prefix _ _ _ _ _) = return (s, [e])

step e@(EventEndElement en) s@(S Accum sn _ _ _ _)
    | en == sn  = toBetween <$> feedEvent e s
    | otherwise = feedEvent e s
step e s@(S Accum _ _ _ _ _) = feedEvent e s

step e@(EventBeginElement en _) s@(S Between sn _ _ _ _)
    | en == sn  = toAccum  <$> startTag e s
    | otherwise = toSuffix <$> flushSample e s
step e s@(S Between _ _ _ _ _) = feedEvent e s

step e s@(S Suffix _ _ _ _ _) = return (s, [e])

-- State transitions

toPrefix, toAccum, toBetween, toSuffix
    :: (SampleState, [Event]) -> (SampleState, [Event])

toPrefix  = set (_1 . sState) Prefix
toAccum   = set (_1 . sState) Accum
toBetween = set (_1 . sState) Between
toSuffix  = set (_1 . sState) Suffix

-- Accumulator/sampler actions

flushSample :: Event -> SampleState -> IO (SampleState, [Event])
flushSample e s =   ( s & sSample .~ uncurry sampleInit (s ^. sSampleState)
                        & sAccum  .~ Nothing
                    ,
                    )
                .   toList
                .   (|> e)
                .   mconcat
                <$> sampleDone (s ^. sSample)

startTag :: Event -> SampleState -> IO (SampleState, [Event])
startTag e s = (, []) <$> maybe (return s') (stepSample s') (s ^. sAccum)
    where
        s' = s & sAccum .~ Just (Seq.singleton e)

        stepSample :: SampleState -> Seq Event -> IO SampleState
        stepSample ss es =   flip (set sSample) ss
                         <$> sampleStep (ss ^. sSample) es

feedEvent :: Event -> SampleState -> IO (SampleState, [Event])
feedEvent e = return . (, []) . over sAccum (fmap (|> e))

-- Fold utilities

stepFold :: F.Fold a b -> a -> F.Fold a b
stepFold (F.Fold f b done) x = F.Fold f (f b x) done

stepFoldM :: Monad m => F.FoldM m a b -> a -> m (F.FoldM m a b)
stepFoldM (F.FoldM f b done) x = do
    b' <- b
    return $ F.FoldM f (f b' x) done

-- Sampling

sample :: (PrimMonad m, Foldable f) => Int -> f a -> Gen (PrimState m) -> m [a]
sample k xs g = F.foldM (sampleInit k g) xs

sampleInit :: PrimMonad m => Int -> Gen (PrimState m) -> Sampler m a
sampleInit k g = F.FoldM (foldStep g) start done
    where
        start = return (M.empty, 1)
        done  = return . fmap snd . M.toAscList . fst

        foldStep :: PrimMonad m
                 => Gen (PrimState m)
                 -> (M.IntMap a, Int)
                 -> a
                 -> m (M.IntMap a, Int)
        foldStep g' (m, n) a
            | n <= k    = return (M.insert n a m, succ n)
            | otherwise = do
                x :: Double <- uniform g'
                if x <= (fromIntegral k / fromIntegral n)
                   then do
                       i <- (M.keys m !!) <$> uniformR (0, M.size m - 1) g'
                       return (M.insert n a $ M.delete i m, succ n)
                   else return (m, succ n)

sampleStep :: Monad m => Sampler m a -> a -> m (Sampler m a)
sampleStep = stepFoldM

sampleDone :: Monad m => Sampler m a -> m [a]
sampleDone (F.FoldM _ b z) = z =<< b

