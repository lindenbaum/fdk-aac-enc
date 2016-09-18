{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Audio.FdkAac (aacEncOpen, aacEncClose, AacEncoderHandle, module X) where

-- import qualified Data.ByteString               as BS
import           Data.ByteString.Mp4.AudioFile as X
import           Data.ByteString.IsoBaseFileFormat.Util.Time as X
import           Data.Type.BitRecords as X
import           Foreign.C.Types
import qualified Language.C.Inline             as C
-- import qualified Language.C.Types              as C
-- import qualified Language.C.Types.Parse        as C
-- import qualified Data.Vector.Storable as V
-- import qualified Data.Vector.Storable.Mutable as VM
-- import           Data.Monoid

C.include "<stdio.h>"
C.include "<stdint.h>"
C.include "fdk-aac/aacenc_lib.h"

newtype AacEncoderHandle = MkAacEncoderHandle CUIntPtr

aacEncOpen :: AacMp4StreamConfig -> IO (Maybe AacEncoderHandle)
aacEncOpen AacMp4StreamConfig{..} =
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
       return NULL;
  } |] >>= (\ hPtr -> if hPtr == 0 then return Nothing else return (Just (MkAacEncoderHandle hPtr)))

aacEncClose :: AacEncoderHandle -> IO Bool
aacEncClose (MkAacEncoderHandle !hPtr) =
    [C.block| int {
       AACENC_ERROR e;
       HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t hPtr);
       return aacEncClose(&phAacEncoder));
    } |]
    >>= return . (== 0)


-- | Take a 'Modulo' buffer and consume some data from it. If an output frame was
-- generated return it together with the updated module buffer.
-- aacEncodePcm
--   :: AacEncoderHandle
--   -> ModuloBuffer Pcm
--   -> (AacEncOutArgs -> IO a) -> IO (Either FdkAacError a)
