{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Database.Etcd.JSON (
  EtcdVersion(..)
, NodeValue(..)
, PreviousValue(..)
, Pair(..)
, Action(..)) where

import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Data.Singletons.TypeLits (Symbol, KnownSymbol, symbolVal)

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


data Action :: Symbol -> * -> * where
  Action :: a -> Action s a
  deriving (Eq, Show)

instance (A.FromJSON a, KnownSymbol s) => A.FromJSON (Action s a) where
  parseJSON o = proxyParse Proxy o
    where
      proxyParse :: (A.FromJSON a, KnownSymbol s) =>
        Proxy s -> A.Value -> Parser (Action s a)
      proxyParse p (A.Object v) = v .: "action" >>= \a ->
        if a == symbolVal p then
          (Action <$> A.parseJSON o)
        else
          empty
      proxyParse _ _ = empty
