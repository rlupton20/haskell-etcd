{-# LANGUAGE OverloadedStrings #-}
module Database.Etcd where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (String)
import qualified Data.Aeson as A
import Control.Monad.Free (Free, liftF, foldFree)

import Database.Etcd.Internal
import Lib.Prelude

type Key = String
type Value = B.ByteString
type TTL = Maybe Integer
type Recursive = Bool
type Wait = Maybe Integer

-- |PrevCond is a set of conditions for atomic compare and swap
data PrevCond = PrevValue String | PrevIndex Integer | PrevExist Bool
type PrevValue = Maybe PrevCond

render :: PrevCond -> String
render (PrevValue t) = "prevValue=" ++ t
render (PrevIndex n) = "prevIndex=" ++ (show n :: [Char])
render (PrevExist True) = "prevExist=true"
render (PrevExist False) = "prevExist=false"

data EtcdA a = Get Key (BL.ByteString -> a)
  | Put Key Value (BL.ByteString -> a)
  | Delete Key (BL.ByteString -> a)

instance Functor EtcdA where
  fmap f (Get k g) = Get k (f . g)
  fmap f (Put k v g) = Put k v (f . g)
  fmap f (Delete k g) = Delete k (f . g)

type EtcdM = Free EtcdA

getJ :: (A.FromJSON a) => Key -> EtcdM (Maybe a)
getJ k = liftF . fmap A.decode $ Get k identity

putJ :: (A.FromJSON a) => Key -> Value -> EtcdM (Maybe a)
putJ k v = liftF . fmap A.decode $ Put k v identity

deleteJ :: (A.FromJSON a) => Key -> EtcdM (Maybe a)
deleteJ k = liftF . fmap A.decode $ Delete k identity

runEtcdA :: Etcd -> EtcdA a -> IO a
runEtcdA e (Get k f) = fmap f $ getIO (unEtcd e) k
runEtcdA e (Put k v f) = fmap f $ putIO (unEtcd e) k v 
runEtcdA e (Delete k f) = fmap f $ deleteIO (unEtcd e) k

runEtcd :: Etcd -> EtcdM a -> IO a
runEtcd e = foldFree (runEtcdA e)
