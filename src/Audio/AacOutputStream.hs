module Audio.AacOutputStream
  ( StreamId(..)
  , type SegmentHandler
  , Segment(..)
  , InitSegment(..)
  , Context()
  , streamOpen
  , streamIncreaseBaseTime
  , streamEncodePcm
  , streamClose
  ) where

import qualified Data.ByteString.Mp4.AudioStreaming as Mp4
import           Audio.FdkAac
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Coerce
import Data.Word
import Data.Maybe (isJust)
import Data.Int
import Data.Time.Clock
import Control.Monad.IO.Class
import Data.Vector.Storable.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Text.Printf
import           Data.Time.Clock               (UTCTime)

streamOpen
  :: MonadIO m
  => StreamId -> UTCTime -> Word32 -> m (Maybe (InitSegment, Context))
streamOpen sId availabilityStartTime segmentDurationMillis =
    liftIO lowLevelInit >>= mapM createContext
  where
    lowLevelInit = do
      let (initBuilder, st) =
            Mp4.streamInitUtc
            (printf "TalkFlow:%0.16X" (unStreamId sId))
            availabilityStartTime
            segmentDurationMillis
            False
            Mp4.SF16000
            Mp4.SingleChannel
      fmap (initBuilder, st, ) <$> aacEncoderNew (Mp4.getStreamConfig st)

    createContext (initBuilder, st, h) = do
      o <- liftIO $ VM.new 768
      let !i = InitSegment (BL.toStrict (BB.toLazyByteString initBuilder))
      return (i, Context h o st sId segmentDurationMillis 0)

streamClose
  :: MonadIO m
  => Context -> m (Maybe Segment)
streamClose Context{..} = liftIO $ do
    aacEncoderClose faHandle  -- TODO error handling
    let (!msegment, faStream') = Mp4.streamFlush faStream
    case msegment of
      Just segment -> do
        let strictSeg = BL.toStrict (BB.toLazyByteString segment)
        printf "Got a last segment with %d bytes\n" (BS.length strictSeg)
        return (Just (mkSegment strictSeg faStream'))
      Nothing ->
        return Nothing

-- TODO fix this from the bottom up...
streamIncreaseBaseTime
  :: forall m . MonadIO m
  => NominalDiffTime
  -> SegmentHandler m
  -> Context
  -> m Context
streamIncreaseBaseTime diffTime callback c@Context{..} =
  do let (!msegment, faStreamFlushed) = Mp4.streamFlush faStream
     case msegment of
      Just segment -> do
        let strictSeg = BL.toStrict (BB.toLazyByteString segment)
        callback (mkSegment strictSeg faStreamFlushed)
      Nothing ->
        return ()
     return c { faStream = Mp4.addToBaseTime faStreamFlushed diffTime }

streamEncodePcm
  :: forall m . MonadIO m
  => V.Vector Int16
  -> SegmentHandler m
  -> Context
  -> m (Maybe Context)
streamEncodePcm !inVecFrozen !callback !ctxIn =
  liftIO (V.thaw (coerce inVecFrozen)) >>= go ctxIn
  where
    go :: Context -> VM.IOVector C.CShort -> m (Maybe Context)
    go !ctx@Context{..} !inVec =
      do (!ok, !cons, !inVec', !outVec')
            <- liftIO $ aacEncoderEncode faHandle inVec faOutVec
         if not ok
           then do liftIO $ printf "\n\n\n*** Encoder error in stream: %16X\n\n\n"
                            (unStreamId faId)
                   -- TODO error handling
                   return Nothing
           else do
             let !totalConsumed = faConsumedButUnencodedSampleCount + cons
             nextFaStream <-
               case outVec' of
                 Nothing ->
                   return faStream
                 Just vo -> do
                   vo' <- liftIO $ V.freeze vo
                   let !voBS = vectorToByteString vo'
                       (!msegment, !faStream') = Mp4.streamNextSample totalConsumed
                                                                      voBS
                                                                      faStream
                   case msegment of
                     Just !segment -> do
                       let !strictSeg = BL.toStrict (BB.toLazyByteString segment)
                       callback (mkSegment strictSeg faStream')
                     Nothing ->
                       return ()
                   return faStream'
             let !nextCtx = ctx { faStream = nextFaStream
                                , faConsumedButUnencodedSampleCount =
                                  if isJust outVec'
                                     then 0
                                     else totalConsumed}
             case inVec' of
               Nothing -> do -- Input consumed, done here
                 return (Just nextCtx)
               Just !inVecRest -> do  -- Still input
                 go nextCtx inVecRest

newtype StreamId = StreamId {unStreamId :: Word64}
  deriving (Integral,Num,Eq,Ord,Show,Enum,Real)

data Context =
  Context
    { faHandle                          :: !AacEncoderHandle
    , faOutVec                          :: !(VM.IOVector C.CUChar)
    , faStream                          :: !Mp4.StreamingContext
    , faId                              :: !StreamId
    , faSegmentDuration                 :: !Word32
    , faConsumedButUnencodedSampleCount :: !Word32
    }

type SegmentHandler m = (Segment -> m ())


mkSegment :: BS.ByteString -> Mp4.StreamingContext -> Segment
mkSegment !b !sc =
  let !acDT = Mp4.getStreamBaseTime sc
      !acSeq = Mp4.getStreamSequence sc
  in Segment acSeq acDT b
