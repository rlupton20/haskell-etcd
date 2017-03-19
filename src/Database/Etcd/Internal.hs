{-# LANGUAGE OverloadedStrings #-}
module Database.Etcd.Internal where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.String (String, fromString)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, setRequestBodyLBS)

import Lib.Prelude

newtype Etcd = Etcd { unEtcd :: String }

etcd :: String -> Etcd
etcd = Etcd

getIO :: String -> IO B.ByteString
getIO url = case parseRequest url of
  Just req -> getResponseBody <$> httpLBS req
  Nothing -> pure ""

putIO :: String -> String -> B.ByteString -> IO B.ByteString
putIO url key value = case parseRequest ("PUT " ++ url) of
  Just req -> let newReq = setRequestBodyLBS "" req in
                getResponseBody <$> httpLBS newReq
  Nothing -> pure ""
