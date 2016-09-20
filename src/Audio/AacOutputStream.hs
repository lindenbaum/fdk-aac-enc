module Audio.AacOutputStream
  ( StreamId(..)
  , type SegmentHandler
  , Segment(..)
  , InitSegment(..)
  , Context()
  , streamOpen
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
import Data.Int
import Control.Monad.IO.Class
import Data.Vector.Storable.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Text.Printf

newtype StreamId = StreamId {unStreamId :: Word64}
  deriving (Integral,Num,Eq,Ord,Show,Enum,Real)

type SegmentHandler m = (Segment -> m ())

data Segment =
  Segment
  { aosSegmentSequence :: !Word32
  , aosSegmentTime     :: !Word32
  , aosSegmentData     :: !BS.ByteString
  }

instance Show Segment where
  show (Segment !s !t !d) =
    printf "seq: %14d - time: %14d - size: %14d" s t (BS.length d)

mkSegment :: BS.ByteString -> Mp4.StreamingContext -> Segment
mkSegment !b !sc =
  let !acDT = Mp4.getStreamBaseTime sc
      !acSeq = Mp4.getStreamSequence sc
  in Segment acSeq acDT b

newtype InitSegment =
  InitSegment
  { fromInitSegment :: BS.ByteString }

instance Show InitSegment where
  show (InitSegment !d) =
    printf "* INIT SEGMENT - size: %14d" (BS.length d)

data Context =
  Context
    { faHandle   :: !AacEncoderHandle
    , faOutVec   :: !(VM.IOVector C.CUChar)
    , faStream   :: !Mp4.StreamingContext
    , faId       :: !StreamId
    --    , faCache    :: !DashStreamCache
    }

streamOpen :: StreamId -> IO (Maybe (InitSegment, Context))
streamOpen sId = do
  (initBuilder, st) <- Mp4.streamInit (printf "TalkFlow:%0.16X" (unStreamId sId))
                                      500
                                      False
                                      Mp4.SF16000
                                      Mp4.SingleChannel
  aacEncoderNew (Mp4.getStreamConfig st) >>= mapM (onEncoderStarted initBuilder st)
  where
    onEncoderStarted initBuilder st h = do
      o <- VM.new 768
      let !i = InitSegment (BL.toStrict (BB.toLazyByteString initBuilder))
      return (i, Context h o st sId)

streamClose :: Context -> IO (Maybe Segment)
streamClose Context{..} = do
    aacEncoderClose faHandle  -- TODO error handling
    let (!msegment, faStream') = Mp4.streamFlush faStream
    case msegment of
      Just segment -> do
        let strictSeg = BL.toStrict (BB.toLazyByteString segment)
        printf "Got a last segment with %d bytes\n" (BS.length strictSeg)
        return (Just (mkSegment strictSeg faStream'))
      Nothing ->
        return Nothing

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
      do (!ok, !consumedSamples, !inVec', !outVec') <- liftIO $ aacEncoderEncode faHandle inVec faOutVec
         if not ok
           then do liftIO $ printf "\n\n\n*** Encoder error in stream: %16X\n\n\n" (unStreamId faId)
                   -- TODO error handling
                   return Nothing
           else do
             nextFaStream <-
               case outVec' of
                 Nothing ->
                   return faStream
                 Just vo -> do
                   vo' <- liftIO $ V.freeze vo
                   let !voBS = vectorToByteString vo'
                       (!msegment, !faStream') = Mp4.streamNextSample consumedSamples voBS faStream
                   case msegment of
                     Just !segment -> do
                       let !strictSeg = BL.toStrict (BB.toLazyByteString segment)
                       callback (mkSegment strictSeg faStream')
                     Nothing ->
                       return ()
                   return faStream'
             let !nextCtx = ctx { faStream = nextFaStream }
             case inVec' of
               Nothing -> do -- Input consumed, done here
                 return (Just nextCtx)
               Just !inVecRest -> do  -- Still input
                 go nextCtx inVecRest
