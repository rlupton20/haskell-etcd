module Database.Etcd.JSON (
EtcdVersion(..) ) where

import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Aeson ((.:))

import Lib.Prelude

data EtcdVersion = EtcdVersion { serverVersion :: Text
                               , clusterVersion :: Text } deriving (Show)

instance A.FromJSON EtcdVersion where
  parseJSON (A.Object v) = EtcdVersion <$> v .: "etcdserver" <*> v .: "etcdcluster"
  parseJSON _ = empty
