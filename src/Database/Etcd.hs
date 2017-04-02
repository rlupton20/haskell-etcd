{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Database.Etcd
(
  -- * Basic types
    Etcd
  , etcd
  , EtcdM
  , runEtcd
  , Key
  -- * High level API
  , get
  , getDirectory
  , waitFromIndex
  , watchDirectoryFromIndex
  -- * Low level API
  , getJ
  , putJ
  , deleteJ ) where

import           Control.Monad.Free (Free, liftF, foldFree)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default (Default(..))
import           Data.Singletons.TypeLits (KnownSymbol)
import           Data.String (String)

import           Database.Etcd.Internal
import           Database.Etcd.JSON
import           Lib.Prelude hiding (get)


--------------------------------------------------------------------------------
-- Basic types and utilities
--------------------------------------------------------------------------------

etcd :: String -> Etcd
etcd = Etcd

type Key = String
type Value = B.ByteString
type EtcdM = Free EtcdA

runEtcd :: Etcd -> EtcdM a -> IO a
runEtcd e = foldFree (runEtcdA e)


--------------------------------------------------------------------------------
-- Basic high level interface
--------------------------------------------------------------------------------

-- | 'get' obtains the value of a key from etcd. Note the
-- 'FromJSON' instance here is applied to the value which is
-- returned, not to the entire JSON.
get :: (A.FromJSON a) => Key -> EtcdM (Maybe a)
get k = fmap unwrap <$> getJ (keys k) def
  where
    unwrap :: NodeValue a -> a
    unwrap (NodeValue x) = x

-- | 'getDirectory' returns the contents of a directory. The entire JSON
-- response is returned here, so provide a suitable FromJSON instance.
getDirectory :: (A.FromJSON a) => Key -> EtcdM (Maybe a)
getDirectory dir = getJ (keys dir)
                   def { recursive = True }


-- | 'waitFromIndex' returns the value of a node when an event happens
-- at or past the index provided.
waitFromIndex :: (A.FromJSON a) => Key -> Integer -> EtcdM (Maybe a)
waitFromIndex k i = fmap unwrap <$> getJ (keys k)
                    def { waitChange = WaitIndex i }
  where
    unwrap :: NodeValue a -> a
    unwrap (NodeValue x) = x


watchDirectoryFromIndex :: (A.FromJSON a) => Key -> Integer -> EtcdM (Maybe a)
watchDirectoryFromIndex dir i = getJ (keys dir)
                                def { waitChange = WaitIndex i
                                    , recursive = True }


--------------------------------------------------------------------------------
-- Dependently typed interfaces
--------------------------------------------------------------------------------


-- | 'waitForActionFromIndex' provides a dependently typed interface
-- for waiting for actions to happen on keys after a certain index
waitForActionFromIndex :: (A.FromJSON a, KnownSymbol s) =>
  Proxy s -> Key -> Integer -> EtcdM (Maybe a)
waitForActionFromIndex p k i = fmap (unwrap p) <$> getJ (keys k)
                               def { waitChange = WaitIndex i }
  where
    unwrap :: Proxy s -> Action s (NodeValue a) -> a
    unwrap _ (Action (NodeValue x)) = x


-- | 'waitForSetFromIndex' Waits for a set of the provided
-- key from the given index
waitForSetFromIndex :: (A.FromJSON a) => Key -> Integer -> EtcdM (Maybe a)
waitForSetFromIndex k i = waitForActionFromIndex (Proxy :: Proxy "set") k i


--------------------------------------------------------------------------------
-- Low level API
--------------------------------------------------------------------------------

-- | 'getJ' does a get request for a key, using 'GetOptions' to modify
-- it in various ways. It will extract parts of the returned JSON
-- according to the type of 'FromJSON' instance specified/inferred.
getJ :: (A.FromJSON a) => Key -> GetOptions -> EtcdM (Maybe a)
getJ k o = liftF . fmap A.decode $ Get k o identity

-- | 'putJ' allows us to put key and value pairs in the etcd store.
putJ :: (A.FromJSON a) => Key -> Value -> EtcdM (Maybe a)
putJ k v = liftF . fmap A.decode $ Put k v identity

-- | 'deleteJ' lets us delete a key from etcd
deleteJ :: (A.FromJSON a) => Key -> EtcdM (Maybe a)
deleteJ k = liftF . fmap A.decode $ Delete k identity



--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

-- Algebra for describing interaction with etcd
data EtcdA a = Get Key GetOptions (BL.ByteString -> a)
  | Put Key Value (BL.ByteString -> a)
  | Delete Key (BL.ByteString -> a)

instance Functor EtcdA where
  fmap f (Get k o g) = Get k o (f . g)
  fmap f (Put k v g) = Put k v (f . g)
  fmap f (Delete k g) = Delete k (f . g)

runEtcdA :: Etcd -> EtcdA a -> IO a
runEtcdA e (Get k o f) = fmap f $ getIO (unEtcd e) k o
runEtcdA e (Put k v f) = fmap f $ putIO (unEtcd e) k v
runEtcdA e (Delete k f) = fmap f $ deleteIO (unEtcd e) k

-- |keys is a helper function to modify URIs to requests
-- into the keyspace
keys :: String -> String
keys = ("v2/keys/"++)
