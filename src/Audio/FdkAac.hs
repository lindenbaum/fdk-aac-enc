module Audio.FdkAac
  (aacEncoderNew, aacEncoderEncode, aacEncoderClose, AacEncoderHandle(..) -- TODO remove (..)
  , module X)
  where

import           Data.ByteString.Mp4.AudioStreaming as X
import           Control.Monad (when)
import           Foreign.C.Types
import qualified Language.C.Inline                  as C
import qualified Data.Vector.Storable.Mutable       as VM

C.context (C.baseCtx <> C.vecCtx)

C.include "<stdio.h>"
C.include "<stdint.h>"
C.include "fdk-aac/aacenc_lib.h"

-- | Handle for a specific encoder, can be created with 'aacEncoderNew'.
newtype AacEncoderHandle = MkAacEncoderHandle CUIntPtr

-- | Allocate a FDK-AAC encoder.
aacEncoderNew :: AacMp4StreamConfig -> IO (Maybe AacEncoderHandle)
aacEncoderNew AacMp4StreamConfig{..} =
  let
    modules, channels, aot, sampleRate', bitRate, sbrMode, signallingMode, channelMode :: CUInt
    !modules = if useHeAac then 3 else 1 -- TODO parametric stereo
    !channels = case channelConfig of
      GasChannelConfig -> 8
      SingleChannel -> 1
      ChannelPair -> 2
      SinglePair -> 3
      SinglePairSingle -> 4
      SinglePairPair -> 5
      SinglePairPairLfe -> 6
      SinglePairPairPairLfe -> 8
    !aot            = if useHeAac then 5 else 2
    !sampleRate'    = sampleRateToNumber sampleRate
    !bitRate        = round (fromIntegral sampleRate'
                             * ((if useHeAac then 0.625 else 1.5) :: Double))
    !sbrMode        = fromIntegral (fromEnum useHeAac)
    !signallingMode = if useHeAac then 2 else 0
    !channelMode    = case channelConfig of
      GasChannelConfig -> 0
      SingleChannel -> 1
      ChannelPair -> 2
      SinglePair -> 3
      SinglePairSingle -> 4
      SinglePairPair -> 5
      SinglePairPairLfe -> 6
      SinglePairPairPairLfe -> 7
  in
  [C.block| uintptr_t {
    AACENC_ERROR e;
    HANDLE_AACENCODER phAacEncoder;

    e = aacEncOpen(&phAacEncoder, $(unsigned int modules), $(unsigned int channels));
    if (e != AACENC_OK) goto e0;

    e = aacEncoder_SetParam(phAacEncoder, AACENC_AOT, (const UINT) $(unsigned int aot));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_AOT'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_BITRATE, (const UINT) $(unsigned int bitRate));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_BITRATE'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_SAMPLERATE, (const UINT) $(unsigned int sampleRate'));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_SAMPLERATE'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_SIGNALING_MODE, (const UINT) $(unsigned int signallingMode));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_SIGNALING_MODE'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_SBR_MODE, (const UINT) $(unsigned int sbrMode));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_SBR_MODE'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_CHANNELMODE, (const UINT) $(unsigned int channelMode));
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_CHANNELMODE'.\n");
      goto e1;
    }

    e = aacEncoder_SetParam(phAacEncoder, AACENC_AFTERBURNER, 1);
    if (e != AACENC_OK) {
      printf("Failed to set encoder parameter 'AACENC_AFTERBURNER'.\n");
      goto e1;
    }

    e = aacEncEncode(phAacEncoder, NULL, NULL, NULL, NULL);
    if (e != AACENC_OK) {
      printf("Failed to apply the configured encoder parameters.\n");
      goto e1;
    }

    printf("Initialized aac encoder:\n • encoder modules: %d\n • encoder channels: %d\n • audio object type: %d\n • sample rate: %d\n • bit rate: %d\n • sbr mode: %d\n • signalling mode: %d\n • channel mode: %d\n",
          $(unsigned int modules),
          $(unsigned int channels),
          $(unsigned int aot),
          $(unsigned int sampleRate'),
          $(unsigned int bitRate),
          $(unsigned int sbrMode),
          $(unsigned int signallingMode),
          $(unsigned int channelMode));

    return ((uintptr_t) phAacEncoder);

    e1:
       if (AACENC_OK != aacEncClose(&phAacEncoder)) {
          printf("Failed to free the AAC Encoder.\n");
       }

    e0:
       printf("FDK-AAC encoder error: %d\n", e);
       return (uintptr_t)NULL;
  } |] >>= (\ hPtr -> if hPtr == 0 then return Nothing else return (Just (MkAacEncoderHandle hPtr)))


-- | Encode mutable IO vectors.
aacEncoderEncode
  :: AacEncoderHandle
  -> VM.IOVector C.CShort
  -> VM.IOVector C.CUChar
  -> IO (Bool, Word32, Maybe (VM.IOVector C.CShort), Maybe (VM.IOVector C.CUChar))
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
  when (retCode /= 0) $ do
     printf "\n\nEncoding Error Report\n"
     printf "=====================\n"
     printf "- exit code:    %8d\n"   $ fromIntegral @C.CInt @Int retCode
     printf "- numOutBytes:  %8d\n"   $ fromIntegral @C.CInt @Int numOutBytes
     printf "- numInSamples: %8d\n"   $ fromIntegral @C.CInt @Int numInSamples
     printf "- numAncBytes:  %8d\n\n" $ fromIntegral @C.CInt @Int numAncBytes
  let !numInSamplesI = fromIntegral @C.CInt @Int numInSamples
  !mRetInBuf <-
      if (numInSamplesI >= VM.length vec) then return Nothing
      else
       do let !inSliceLen = VM.length vec - numInSamplesI -- TODO what about stereo!?
              !inSlice    = VM.slice numInSamplesI inSliceLen vec
          printf "OPTIMIZE-ME: consumed only %d of %d input samples, returning input slice of size %d.\n"
                 numInSamplesI (VM.length vec) inSliceLen
          return (Just inSlice)
  let !numOutBytesI = fromIntegral @C.CInt @Int numOutBytes
      !mRetOutBuf   = if (numOutBytesI == 0)
                      then Nothing
                      else Just  (VM.slice 0 numOutBytesI bso)
  return (retCode == 0, fromIntegral numInSamplesI, mRetInBuf, mRetOutBuf)

-- TODO FLUSH!!!!!!!!!!!!!!!!!!!!!!


-- | Close an FDK-AAC encoder.
aacEncoderClose :: AacEncoderHandle -> IO ()
aacEncoderClose (MkAacEncoderHandle !hPtr) = do
   res <- [C.block| int {
             AACENC_ERROR e;
             HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t hPtr);
             return aacEncClose(&phAacEncoder);
          } |]
   when (res /= 0)
     (putStrLn "\n\n\n*** ERROR: Failed to close FDK AAC Encoder! *** \n\n\n")
