module Audio.X where

-- import qualified Data.ByteString               as BS
import           Foreign.ForeignPtr           as C
-- import           Foreign.Marshal.Alloc        as C
import qualified Language.C.Inline            as C

-- import qualified Language.C.Types             as C
-- import qualified Language.C.Types.Parse        as C
import           Audio.FdkAac
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Coerce


C.context (C.baseCtx <> C.vecCtx)

C.include "<stdio.h>"
C.include "<stdint.h>"
C.include "fdk-aac/aacenc_lib.h"

newtype AacEncBuffDesc =
  AacEncBuffDesc (ForeignPtr AacEncBuffDesc)

aacEncoderEncode
  :: AacEncoderHandle
  -> VM.IOVector C.CShort
  -> VM.IOVector C.CUChar
  -> IO (Bool, Maybe (VM.IOVector C.CShort), Maybe (VM.IOVector C.CUChar))
aacEncoderEncode (MkAacEncoderHandle !h) !vec !bso = do
  ((numOutBytes, numInSamples, numAncBytes), retCode)
     <- C.withPtrs $
       \ (numOutBytesP, numInSamplesP, numAncBytesP) ->
         [C.block| int {
            AACENC_ERROR e;
            HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t h);

            /* Input buffer */
            AACENC_BufDesc inBuffDesc;
            INT inBuffIds[1]             = {IN_AUDIO_DATA};
            INT inBuffSizes[1]           = {$vec-len:vec * 2};
            INT inBuffElSizes[1]         = {2};
            void* inBuffBuffers[1]       = {$vec-ptr:(short *vec)};
            inBuffDesc.numBufs           = 1;
            inBuffDesc.bufs              = &inBuffBuffers;
            inBuffDesc.bufferIdentifiers = &inBuffIds;
            inBuffDesc.bufSizes          = &inBuffSizes;
            inBuffDesc.bufElSizes        = &inBuffElSizes;
            AACENC_InArgs inArgs         = { .numInSamples = $vec-len:vec, .numAncBytes = 0 };

            /* Ouput buffer */
            AACENC_BufDesc outBuffDesc;
            INT outBuffIds[1]             = {OUT_BITSTREAM_DATA};
            INT outBuffSizes[1]           = {$vec-len:bso};
            INT outBuffElSizes[1]         = {1};
            void* outBuffBuffers[1]       = {$vec-ptr:(unsigned char *bso)};
            outBuffDesc.numBufs           = 1;
            outBuffDesc.bufs              = &outBuffBuffers;
            outBuffDesc.bufferIdentifiers = &outBuffIds;
            outBuffDesc.bufSizes          = &outBuffSizes;
            outBuffDesc.bufElSizes        = &outBuffElSizes;
            AACENC_OutArgs outArgs;

            e = aacEncEncode (phAacEncoder, &inBuffDesc, &outBuffDesc,
                              &inArgs, &outArgs);

            *($(int* numOutBytesP)) = outArgs.numOutBytes;
            *($(int* numInSamplesP)) = outArgs.numInSamples;
            *($(int* numAncBytesP)) = outArgs.numAncBytes;

            return e;
         }|]
  printf "Encoding report\n"
  printf "===============\n"
  printf "- exit code:    %8d\n" $ fromIntegral @C.CInt @Int retCode
  printf "- numOutBytes:  %8d\n" $ fromIntegral @C.CInt @Int numOutBytes
  printf "- numInSamples: %8d\n" $ fromIntegral @C.CInt @Int numInSamples
  printf "- numAncBytes:  %8d\n" $ fromIntegral @C.CInt @Int numAncBytes



  let !numInSamplesI = fromIntegral @C.CInt @Int numInSamples
  !mRetInBuf <-
      if (numInSamplesI >= VM.length vec) then return Nothing
      else
       do let !inSliceLen = VM.length vec - numInSamplesI
              !inSlice    = VM.slice numInSamplesI inSliceLen vec
          printf "NOTE: consumed only %d of %d input samples, returning input slice of size %d.\n"
                 numInSamplesI (VM.length vec) inSliceLen
          return (Just inSlice)

  let !numOutBytesI = fromIntegral @C.CInt @Int numOutBytes
  !mRetOutBuf <-
      if (numOutBytesI == 0) then return Nothing
      else
       do let !outSlice = VM.slice 0 numOutBytesI bso
          printf "NOTE: Generated %d bytes of output.\n"
                 numOutBytesI
          return (Just outSlice)

  return (retCode == 0, mRetInBuf, mRetOutBuf)


lilltest = do
  let vinOrig = V.fromList [0..4097]
      vinOrig :: V.Vector Int16
  now <- mp4CurrentTime
  (Just h) <- aacEncoderNew (AacMp4StreamConfig now "Lill' Test" 0 False SF48000 SingleChannel)

  (vin :: VM.IOVector C.CShort) <- V.thaw (coerce vinOrig)
  bso <- VM.new 768

  (ok,Just vin',Just bso') <- aacEncoderEncode h vin bso

  putStrLn "Remaining input: "
  printHexList vin'

  putStrLn "Output: "
  printHexList bso'

  (ok,vin'',bso'') <- aacEncoderEncode h vin' bso

  putStrLn "Remaining input: (2)"
  mapM_ printHexList vin''

  putStrLn "Output: (2)"
  mapM_ printHexList bso''

  aacEncoderClose h

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
    streams :: TVar (Map.Map StreamId StreamCtx)
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
