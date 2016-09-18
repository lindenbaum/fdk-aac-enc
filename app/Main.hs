{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Audio.FdkAac
-- import           Data.ByteString.IsoBaseFileFormat.Util.Time
-- import           Data.ByteString.Mp4.AudioFile
-- import           Data.Type.BitRecords

main :: IO ()
main =
  do creationTime <- mp4CurrentTime
     Just h <- aacEncOpen (AacMp4StreamConfig
                           creationTime
                           "Lindenbaum TalkFlow"
                           0
                           False
                           SF16000
                           SingleChannel)
     True <- aacEncClose h
     return ()
