module Audio.AacOut  where

import qualified Language.C.Inline            as C
import           Audio.FdkAac
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Coerce
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Vector.Storable.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB


-- data TalkFlowServerContext =
--   TalkFlowServerContext
--   { dashStreams :: DashStreams
--   , rtpPorts    :: TVar (Set.Set Int)
--   }

-- data DashStreams =
--    DashStreams
--      { streamInitSegment     :: TVar (Map.Map AacOutStreamId BS.ByteString)
--      , streamAudioSegments   :: TVar (Map.Map AacOutStreamId (Set.Set (Word32, BS.ByteString))
--      , streamManifests       :: TVar (Map.Map AacOutStreamId BS.ByteString)
--      , streamCurrentSegments :: TVar (Map.Map AacOutStreamId Word32)
--      }

newtype AacOutStreamId = AacOutStreamId {unAacOutStreamId :: Word64}
  deriving (Integral,Num,Eq,Ord,Show,Enum,Real)

type AacOutStreamSegmentHandler m = (AacOutStreamSegment -> m ())

data AacOutStreamSegment =
  AacOutStreamSegment
  { aosSegmentSequence :: !Word32
  , aosSegmentTime     :: !Word32
  , aosSegmentData     :: !BS.ByteString
  }

instance Show AacOutStreamSegment where
  show (AacOutStreamSegment !s !t !d) =
    printf "seq: %14d - time: %14d - size: %14d\n" s t (BS.length d)

mkAacOutStreamSegment :: BS.ByteString -> StreamingContext -> AacOutStreamSegment
mkAacOutStreamSegment !b !sc =
  let !acDT = getStreamBaseTime sc
      !acSeq = getStreamSequence sc
  in AacOutStreamSegment acSeq acDT b

newtype AacOutStreamInitSegment =
  AacOutStreamInitSegment
  { fromAacOutStreamInitSegment :: BS.ByteString }

instance Show AacOutStreamInitSegment where
  show (AacOutStreamInitSegment !d) =
    printf "* INIT SEGMENT - size: %14d\n" (BS.length d)

data AacOutStreamContext =
  AacOutStreamContext
    { faHandle   :: !AacEncoderHandle
    , faOutVec   :: !(VM.IOVector C.CUChar)
    , faStream   :: !StreamingContext
    , faId       :: !AacOutStreamId
    --    , faCache    :: !DashStreamCache
    }

aacOutStreamOpen :: AacOutStreamId -> IO (Maybe (AacOutStreamInitSegment, AacOutStreamContext))
aacOutStreamOpen sId = do
  (initBuilder, st) <- streamInit (printf "TalkFlow:%0.16X" (unAacOutStreamId sId))
                                  500
                                  False
                                  SF16000
                                  SingleChannel
  aacEncoderNew (getStreamConfig st) >>= mapM (onEncoderStarted initBuilder st)
  where
    onEncoderStarted initBuilder st h = do
      o <- VM.new 768
      let !i = AacOutStreamInitSegment (BL.toStrict (BB.toLazyByteString initBuilder))
      return (i, AacOutStreamContext h o st sId)

aacOutStreamClose :: AacOutStreamContext -> IO (Maybe AacOutStreamSegment)
aacOutStreamClose AacOutStreamContext{..} = do
    aacEncoderClose faHandle  -- TODO error handling
    let (!msegment, faStream') = streamFlush faStream
    case msegment of
      Just segment -> do
        let strictSeg = BL.toStrict (BB.toLazyByteString segment)
        printf "Got a last segment with %d bytes\n" (BS.length strictSeg)
        return (Just (mkAacOutStreamSegment strictSeg faStream'))
      Nothing ->
        return Nothing

aacOutStreamEncodePcm
  :: forall m . MonadIO m
  => V.Vector Int16
  -> AacOutStreamSegmentHandler m
  -> AacOutStreamContext
  -> m (Maybe AacOutStreamContext)
aacOutStreamEncodePcm !inVecFrozen !callback !ctxIn =
  liftIO (V.thaw (coerce inVecFrozen)) >>= go ctxIn
  where
    go :: AacOutStreamContext -> VM.IOVector C.CShort -> m (Maybe AacOutStreamContext)
    go !ctx@AacOutStreamContext{..} !inVec =
      do (!ok, !consumedSamples, !inVec', !outVec') <- liftIO $ aacEncoderEncode faHandle inVec faOutVec
         if not ok
           then do liftIO $ printf "\n\n\n*** Encoder error in stream: %16X\n\n\n" (unAacOutStreamId faId)
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
                       (!msegment, !faStream') = streamNextSample consumedSamples voBS faStream
                   case msegment of
                     Just !segment -> do
                       let !strictSeg = BL.toStrict (BB.toLazyByteString segment)
                       callback (mkAacOutStreamSegment strictSeg faStream')
                     Nothing ->
                       return ()
                   return faStream'
             let !nextCtx = ctx { faStream = nextFaStream }
             case inVec' of
               Nothing -> do -- Input consumed, done here
                 return (Just nextCtx)
               Just !inVecRest -> do  -- Still input
                 go nextCtx inVecRest


lilltest2 = do
    Just (initseg, ctx) <- aacOutStreamOpen 123
    print initseg
    let vin = V.fromList [0..1023]
        vin :: V.Vector Int16
    ctx' <- go (10000 :: Word32) ctx vin
    o <- aacOutStreamClose ctx'
    case o of
      Just lastSegm -> print lastSegm
      Nothing -> return ()
  where
    go 0 ac _vin = return ac
    go n ac vin = do
      Just ac' <- aacOutStreamEncodePcm vin print ac
      go (n-1) ac' vin

printHexList :: forall a . (VM.Storable a, Integral a) => VM.IOVector a -> IO ()
printHexList v = do
  v' <- V.unsafeFreeze v
  putStrLn (unwords (printf "%x" . fromIntegral @a @Int <$> V.toList v'))



{-

to wrap up (literally ;) ) ===

for evey encoder buffer make a 'trun' entry:

sample length
  how long decoded?

sample size
  how long encoded?

sample flags
  some flags, mainly set the 'sample_depends_on' to '2'
  indicating this is an independent sample

gather any number of 'trun' entries together, calculate the
offset into the file (trun count * trun entry len + trun headers + moof header...)


it will be like this

s <- boxSize moofBox


   stypeBox iso5stype
:. moof
  :. mfhd
  :| traf
      :| tfhd <- flags = 0x020000 default-base-is-moof + track id
      :| tfdt <- the decoding time of this run
      :| trun <- flags = 0x701 - sample count / data_offset /
                 [ sample_dur,
                   sample_size,
                   sample_flags = 0x02000000 (independent sample) ]
:| mdat

NOTE: the data_offset will be computed unsafe and independently and passed in as
parameter.

---------

Overall architecture:


data ServerCtx =
  MkServerCtx {
    rtpPorts :: TVar (Set.Set Int)
    streams :: TVar (Map.Map AacOutStreamId StreamCtx)
  }

server serverConfig = do
   serverCtx <- newServerCtx

   concurrently $
      (httpServer serverConfig serverCtx)
      (controlServer serverConfig serverCtx)

controlServer serverCfg serverCtx =
 runTCPServer (controlServerSettings serverConfig) $ \client ->
    bracketP
      (initStreamCtx serverConfig serverCtx client)
      terminateStreamCtx
      runStream


initStreamCtx sCfg sCtx client = do
  atomically $ do
   rtp <- takeRtpPort (sCtx rtpPorts)


streamer serverConfig serverCtx client = do
  streamCtx <- newTVar


  concurrently $
    (readStreamControlTrafic serverCtx streamCtx)
    (encodeRTP serverCtx streamCtx $$ segmenter serverCtx streamCtx)

streamController
  communicate with the client via TCP (or erl-interface)
  messages: -> CONNECT / SET_DEBUG_INFO / DISCONNECT
            <- RTP_PORT_OPEN / STREAM_URL_AVAILABLE

streamEncoder
  receive RTP, and encode it, send it to the

streamSegmenter
  gather enough samples to put into a single packet for http streaming
  create the segment box
  yield segments to the segment cache

segment cache
  a TVar with a Set of segments
  a TVar with the init.mp4 segment
  a TVar with the init.mp4 segment

http server
  /streams/${stream-id}/manifest.mpd
  /streams/${stream-id}/16kMono/init.mp4
  /streams/${stream-id}/16kMono/${frame-index}.m4s


  read segments from the segment cache

-}
