{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module Database.Etcd.GenericJSON where

import Data.Text (pack)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Data.Singletons (Sing, SingI(..))
import Data.Singletons.TypeLits (Symbol, SSymbol, KnownSymbol, withKnownSymbol, symbolVal)
import Data.Singletons.Prelude.List (Sing(SNil, SCons))
import Data.String (String)

import Lib.Prelude

data JSBranch :: [Symbol] -> * -> * where
  JSNil :: a -> JSBranch '[] a
  JSCons :: JSBranch s a -> JSBranch (t ': s) a

unwrap :: JSBranch ks a -> a
unwrap jsb = case jsb of
  JSNil x -> x
  JSCons jsb' -> unwrap jsb'

instance (A.FromJSON a, SingI ls) => A.FromJSON (JSBranch ls a) where
  parseJSON :: (A.FromJSON a, SingI ls) => A.Value -> Parser (JSBranch ls a)
  parseJSON o = case o of
    (A.Object v) -> parseSing sing v
    _ -> empty
    where
      parseSing :: Sing is -> A.Object -> Parser (JSBranch is a)
      parseSing s v = case s of
        SNil -> JSNil <$> A.parseJSON (A.Object v)
        i `SCons` iss -> let key = pack (symToVal i) in
          JSCons <$> (v .: key >>= (parseSing iss))

symToVal :: SSymbol s -> String
symToVal s = withKnownSymbol s $ proxySym s Proxy
  where
    proxySym :: (KnownSymbol n) => SSymbol n -> Proxy n -> String
    proxySym _ p = symbolVal p
