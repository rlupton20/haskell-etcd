{-# LANGUAGE OverloadedStrings #-}
module Database.Etcd.Internal where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (String)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, setRequestMethod, setRequestBodyURLEncoded)

import Lib.Prelude

newtype Etcd = Etcd { unEtcd :: String }

etcd :: String -> Etcd
etcd = Etcd

getIO :: String -> String -> IO BL.ByteString
getIO url key = case parseRequest (url ++ key) of
  Just req -> getResponseBody <$> httpLBS req
  Nothing -> pure ""

putIO :: String -> String -> B.ByteString -> IO BL.ByteString
putIO url key value = case parseRequest (url ++ key) of
  Just req -> let newReq = setRequestMethod "PUT" $
                    setRequestBodyURLEncoded [("value", value)] req in
                putStrLn (show newReq :: String) >>
                getResponseBody <$> httpLBS newReq
  Nothing -> pure ""
