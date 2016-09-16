{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString   as BS
import           Foreign.C.Types
import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "fdk-aac/aacenc_lib.h"

main :: IO ()
main = do
  x <- [C.block| int {
         return 0;
       } |]
  print x
