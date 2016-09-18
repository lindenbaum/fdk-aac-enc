{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simulate a /modulo buffer/.
module Audio.ModuloBufferPlaypen where

import           Audio.Event
import           Audio.Pcm
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BL
import           Data.Conduit
import           Data.Conduit.Network.UDP
import           Data.Streaming.Network
import           Network.Socket               (close)
import           Text.Printf

rtpSource
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m Message
rtpSource !port !host = do
  bracketP (bindPortUDP port host) close
    (\sock -> sourceSocket sock 1024)

udpDbgConduit
  :: MonadIO m
  => Conduit Message m BL.ByteString
udpDbgConduit = awaitForever $ \ !m ->
  do
    let !d = msgData m
    liftIO (printf (show (BL.length d)))
    yield d

fakeSource :: Source IO (Event Pcm)
fakeSource = do
  s <- liftIO (putStrLn "Fake Source: Next Input buffer? " >> (reads <$> getLine))
  case s of
    [(seqNum, _)] ->
      yield (Event seqNum (pcmFromList [0..159])) >> fakeSource
    _ ->
      return ()

fakeEncoder :: Sink (Part (Event Pcm)) IO ()
fakeEncoder = do
  ib <- awaitPart
  case ib of
    Nothing ->
      liftIO $ putStrLn "Fake Encoder: Done."
    (Just (off, e@(Event eseq pcm))) -> do
      let sampleCount = pcmLength pcm - off
      liftIO $ printf "Fake Encoder: Event %d - Got %d samples." eseq sampleCount
      liftIO $ putStrLn "How many should I consume?"
      n <- read <$> liftIO getLine
      when (n < sampleCount)
        (leftoverPart e (n + off))
      fakeEncoder

simpleTest1 :: IO ()
simpleTest1 = fakeSource $$ dejitter jb =$ parts =$ fakeEncoder
  where
    jb :: JitterBuffer Pcm
    !jb = emptyJitterBuffer 2 4 0
