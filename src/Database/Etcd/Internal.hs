{-# LANGUAGE OverloadedStrings #-}
module Database.Etcd.Internal where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (String)
import Network.HTTP.Simple (Request, httpLBS, parseRequest
                           , getResponseBody, setRequestMethod
                           , setRequestBodyURLEncoded)
import Data.Default (Default(..))
import Control.Monad.Catch (MonadThrow)

import Lib.Prelude

newtype Etcd = Etcd { unEtcd :: String }

etcd :: String -> Etcd
etcd = Etcd


-- Various options to control the different requests
data Wait = No | WaitIndex Integer | Wait deriving (Eq, Show)

-- |PrevCond is a set of conditions for atomic compare and swap
data PrevCond = PrevValue String | PrevIndex Integer | PrevExist Bool
type PrevValue = Maybe PrevCond

data GetOptions = GetOptions { waitChange :: Wait } deriving (Eq, Show) 

instance Default GetOptions where
  def = GetOptions No

buildGetRequest :: (MonadThrow m) => String -> String -> GetOptions -> m Request
buildGetRequest url key o = parseRequest $ url ++ key ++
                            if null (queries o) then "" else "?" ++
                            queries o
  where
    queries :: GetOptions -> String
    queries = waits

    waits :: GetOptions -> String
    waits o = case waitChange o of
      No -> ""
      WaitIndex n -> "wait=true&waitIndex=" ++ show n
      Wait -> "wait=true"


getIO :: String -> String -> GetOptions -> IO BL.ByteString
getIO url key o = case buildGetRequest url key o of
  Just req -> getResponseBody <$> httpLBS req
  Nothing -> pure ""

putIO :: String -> String -> B.ByteString -> IO BL.ByteString
putIO url key value = case parseRequest (url ++ key) of
  Just req -> let newReq = setRequestMethod "PUT" $
                    setRequestBodyURLEncoded [("value", value)] req in
                getResponseBody <$> httpLBS newReq
  Nothing -> pure ""

deleteIO :: String -> String -> IO BL.ByteString
deleteIO url key = case parseRequest (url ++ key) of
  Just req -> let newReq = setRequestMethod "DELETE" req in
    getResponseBody <$> httpLBS newReq
  Nothing -> pure ""
