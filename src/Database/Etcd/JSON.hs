module Database.Etcd.JSON (
  EtcdVersion(..)
, Value(..)
, PreviousValue(..)
, Pair(..)) where

import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Aeson ((.:))

import Lib.Prelude

data EtcdVersion = EtcdVersion { serverVersion :: Text
                               , clusterVersion :: Text } deriving (Show, Eq)

instance A.FromJSON EtcdVersion where
  parseJSON (A.Object v) = EtcdVersion <$>
    v .: "etcdserver" <*>
    v .: "etcdcluster"
  parseJSON _ = empty

newtype Value a = Value a deriving (Show, Eq)

instance (A.FromJSON a) => A.FromJSON (Value a) where
  parseJSON (A.Object v) = Value <$> (v .: "node" >>= (.: "value"))
  parseJSON _ = empty

newtype PreviousValue a = PreviousValue a deriving (Eq, Show)

instance (A.FromJSON a) => A.FromJSON (PreviousValue a) where
  parseJSON (A.Object v) = PreviousValue <$>
    (v .: "prevNode" >>= (.: "value"))
  parseJSON _ = empty

data Pair a b = Pair {-# UNPACK #-} !a {-# UNPACK #-} !b deriving (Eq, Show)

instance (A.FromJSON a, A.FromJSON b) => A.FromJSON (Pair a b) where
  parseJSON o = Pair <$> A.parseJSON o <*> A.parseJSON o
