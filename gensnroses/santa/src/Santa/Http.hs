{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Santa.Http (
    httpGet
  ) where

import qualified Data.ByteString.Lazy as BSL

import           Network.HTTP.Client (Response)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HTTP

import           Santa.Prelude

httpGet :: HC.Manager -> String -> IO (Response BSL.ByteString)
httpGet mgr url = do
  req <- HC.parseRequest url
  flip HC.httpLbs mgr req {
      HC.requestHeaders = [(,) HTTP.hUserAgent "Santa: Haskell Code Slurper"]
    }
