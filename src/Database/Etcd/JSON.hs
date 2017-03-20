module Database.Etcd.JSON (
  EtcdVersion(..)
, NodeValue(..)
, PreviousValue(..)
, Pair(..)
, ActionSet(..)) where

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


newtype NodeValue a = NodeValue a deriving (Show, Eq)

instance (A.FromJSON a) => A.FromJSON (NodeValue a) where
  parseJSON (A.Object v) = NodeValue <$> (v .: "node" >>= (.: "value"))
  parseJSON _ = empty


newtype PreviousValue a = PreviousValue a deriving (Eq, Show)

instance (A.FromJSON a) => A.FromJSON (PreviousValue a) where
  parseJSON (A.Object v) = PreviousValue <$>
    (v .: "prevNode" >>= (.: "value"))
  parseJSON _ = empty


data Pair a b = Pair !a !b deriving (Eq, Show)

instance (A.FromJSON a, A.FromJSON b) => A.FromJSON (Pair a b) where
  parseJSON o = Pair <$> A.parseJSON o <*> A.parseJSON o


newtype ActionSet a = ActionSet a deriving (Eq, Show)

instance (A.FromJSON a) => A.FromJSON (ActionSet a) where
  parseJSON o@(A.Object v) = v .: "action" >>= \a ->
    if a == ("set" :: Text) then
      (ActionSet <$> A.parseJSON o)
    else
      empty
  parseJSON _ = empty
