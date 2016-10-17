module Audio.FdkAac
  (aacEncoderNew, aacEncoderEncode, aacEncoderClose, AacEncoderHandle)
  where

import           Data.ByteString.Mp4.AudioStreaming
import           Control.Monad (when)
import           Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import           Foreign.C.Types
import qualified Language.C.Inline                  as C
import qualified Data.Vector.Storable.Mutable       as VM
import qualified Data.Vector.Storable               as V

C.context (C.baseCtx <> C.vecCtx)

C.include "<stdio.h>"
C.include "<stdint.h>"
C.include "fdk-aac/aacenc_lib.h"

-- | Handle for a specific encoder, can be created with 'aacEncoderNew'.
data AacEncoderHandle =
  MkAacEncoderHandle
   { encoderHandle :: CUIntPtr
   , unsafeOutBuffer :: VM.IOVector CUChar
   , channelCount :: CUInt
   , encoderDelay :: Word32
   , frameSize :: Word32
   , audioSpecificConfig :: V.Vector CUChar
   }

-- | Allocate a FDK-AAC encoder.
aacEncoderNew :: AacMp4StreamConfig -> IO (Maybe AacEncoderHandle)
aacEncoderNew AacMp4StreamConfig{..} =
  do  let   modules, channels, aot, sampleRate', bitRate, sbrMode, signallingMode, channelMode :: CUInt
            !modules = 0x17
            !channels = case channelConfig of
              GasChannelConfig -> 0
              SingleChannel -> 1
              ChannelPair -> 2
              SinglePair -> 3
              SinglePairSingle -> 4
              SinglePairPair -> 5
              SinglePairPairLfe -> 6
              SinglePairPairPairLfe -> 8
            !aot            = if useHeAac then 5 else 2
            !sampleRate'    = sampleRateToNumber sampleRate
            !bitRate        = channelMode * round (fromIntegral sampleRate' * ((if useHeAac then 0.625 else 1.5) :: Double)) --
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
      let confBufMaxLen = 255
      confBufC <- mallocForeignPtrBytes (fromIntegral confBufMaxLen)
      ((encDelayC
       ,confBufSizeC
       ,frameSizeC)
       ,hPtr) <-
        withForeignPtr confBufC $ \confBufP ->
          C.withPtrs $ \(encDelayP, confBufSizeP, frameSizeP) ->
            [C.block| uintptr_t {
              AACENC_ERROR e;
              HANDLE_AACENCODER phAacEncoder;
              AACENC_InfoStruct pInfo;
              CHANNEL_MODE channelMode;

              switch ($(unsigned int channelMode)) {
                  case 1: channelMode = MODE_1;       break;
                  case 2: channelMode = MODE_2;       break;
                  case 3: channelMode = MODE_1_2;     break;
                  case 4: channelMode = MODE_1_2_1;   break;
                  case 5: channelMode = MODE_1_2_2;   break;
                  case 6: channelMode = MODE_1_2_2_1; break;
              }
              e = aacEncOpen(&phAacEncoder, $(unsigned int modules), $(unsigned int channels));
              if (e != AACENC_OK) goto e0;

              e = aacEncoder_SetParam(phAacEncoder, AACENC_AOT, (const UINT) $(unsigned int aot));
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_AOT'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_SBR_MODE, (const UINT) $(unsigned int sbrMode));
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_SBR_MODE'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_SAMPLERATE, (const UINT) $(unsigned int sampleRate'));
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_SAMPLERATE'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_CHANNELMODE, channelMode);
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_CHANNELMODE'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_BITRATEMODE, 0);
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_BITRATEMODE'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_BITRATE, (const UINT) $(unsigned int bitRate));
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_BITRATE'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_BANDWIDTH, 8000); /* TODO remote hardcoded value */
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_BANDWIDTH'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_TRANSMUX, TT_MP4_RAW);
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_TRANSMUX'.\n");
                goto e1;
              }

              e = aacEncoder_SetParam(phAacEncoder, AACENC_SIGNALING_MODE, (const UINT) $(unsigned int signallingMode));
              if (e != AACENC_OK) {
                printf("Failed to set encoder parameter 'AACENC_SIGNALING_MODE'.\n");
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

              e = aacEncInfo(phAacEncoder, &pInfo);
              if (e != AACENC_OK) {
                printf("Failed to read the encoder info.\n");
                goto e1;
              }

              printf("Initialized aac encoder:\n * encoder modules: %d\n * encoder channels: %d\n * audio object type: %d\n * sample rate: %d\n * bit rate: %d\n * sbr mode: %d\n * signalling mode: %d\n * channel mode: %d\n",
                    $(unsigned int modules),
                    $(unsigned int channels),
                    $(unsigned int aot),
                    $(unsigned int sampleRate'),
                    $(unsigned int bitRate),
                    $(unsigned int sbrMode),
                    $(unsigned int signallingMode),
                    $(unsigned int channelMode));
              for (int i = 0; i < pInfo.confSize && i < $(int confBufMaxLen); ++i) {
                 printf (" * ASC[%d] %8.0x\n", i,  pInfo.confBuf[i]);
                 *($(unsigned char* confBufP) + i) = pInfo.confBuf[i];
              }
              *($(unsigned int* confBufSizeP)) = pInfo.confSize;

              printf (" * delay %d\n", pInfo.encoderDelay);
              *($(unsigned int* encDelayP))  = pInfo.encoderDelay;

              printf (" * samples per channel in each frame %d\n", pInfo.frameLength);
              *($(unsigned int* frameSizeP))  = pInfo.frameLength;


              return ((uintptr_t) phAacEncoder);

              e1:
                 if (AACENC_OK != aacEncClose(&phAacEncoder)) {
                    printf("Failed to free the AAC Encoder.\n");
                 }

              e0:
                 printf("FDK-AAC encoder error: %d\n", e);
                 return (uintptr_t)NULL;
            } |]
      if hPtr == 0
        then return Nothing
        else
          do let ascVM = VM.unsafeFromForeignPtr0 confBufC (fromIntegral confBufSizeC)
             asc <- V.freeze ascVM
             let outSize = fromIntegral (768 * channels)
             outV <- VM.new outSize
             return $ Just $ MkAacEncoderHandle { encoderHandle = hPtr
                                                , channelCount = channels
                                                , encoderDelay = fromIntegral encDelayC
                                                , frameSize = fromIntegral frameSizeC
                                                , unsafeOutBuffer = outV
                                                , audioSpecificConfig = asc}

-- | Encode Samples.
aacEncoderEncode
  :: AacEncoderHandle
  -> V.Vector C.CShort
  -> IO (Bool, Word32, Maybe (V.Vector C.CShort), Maybe (V.Vector C.CUChar))
aacEncoderEncode MkAacEncoderHandle{encoderHandle, unsafeOutBuffer, channelCount} !vec = do
  ((numOutBytes, numInSamples, numAncBytes), retCode)
     <- C.withPtrs $
       \ (numOutBytesP, numInSamplesP, numAncBytesP) ->
         [C.block| int {
            AACENC_ERROR e;
            HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);

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
            AACENC_InArgs inArgs         =
              { .numInSamples = $vec-len:vec
              , .numAncBytes = 0 };

            /* Ouput buffer */
            AACENC_BufDesc outBuffDesc;
            INT outBuffIds[1]             = {OUT_BITSTREAM_DATA};
            INT outBuffSizes[1]           = {$vec-len:unsafeOutBuffer};
            INT outBuffElSizes[1]         = {1};
            void* outBuffBuffers[1]       = {$vec-ptr:(unsigned char *unsafeOutBuffer)};
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
      if (numInSamplesI >= V.length vec) then return Nothing
      else
       do let !inSliceLen = V.length vec - numInSamplesI
              !inSlice    = V.slice numInSamplesI inSliceLen vec
          return (Just inSlice)
  let !numOutBytesI = fromIntegral @C.CInt @Int numOutBytes
  !mRetOutBuf <- if (numOutBytesI == 0)
                      then return Nothing
                      else do !outTooLarge <- V.freeze unsafeOutBuffer
                              let !out = V.slice 0 numOutBytesI outTooLarge
                              return $ Just out
  return (retCode == 0, fromIntegral numInSamplesI `div` fromIntegral channelCount, mRetInBuf, mRetOutBuf)

-- TODO FLUSH!!!!!!!!!!!!!!!!!!!!!!


-- | Close an FDK-AAC encoder.
aacEncoderClose :: AacEncoderHandle -> IO ()
aacEncoderClose MkAacEncoderHandle{encoderHandle} = do
   res <- [C.block| int {
             AACENC_ERROR e;
             HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);
             return aacEncClose(&phAacEncoder);
          } |]
   when (res /= 0)
     (putStrLn "\n\n\n*** ERROR: Failed to close FDK AAC Encoder! *** \n\n\n")
