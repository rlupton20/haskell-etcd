{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Database.Etcd.GenericJSON where

import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import           Data.Singletons (Sing, SingI(..))
import           Data.Singletons.Prelude.List (Sing(SNil, SCons))
import           Data.Singletons.TypeLits ( Symbol, SSymbol, KnownSymbol
                                          , withKnownSymbol, symbolVal )
import           Data.String (String)
import           Data.Text (pack)

import           Lib.Prelude


data JSBranch :: [Symbol] -> * -> * where
  JSNil :: a -> JSBranch '[] a
  JSCons :: JSBranch xs a -> JSBranch (t ': xs) a


unwrap :: JSBranch ixs a -> a
unwrap jsb = case jsb of
  JSNil x -> x
  JSCons jsb' -> unwrap jsb'


instance (A.FromJSON a, SingI xs) => A.FromJSON (JSBranch xs a) where
  parseJSON :: (A.FromJSON a, SingI xs) => A.Value -> Parser (JSBranch xs a)
  parseJSON = parseSing sing
    where
      parseSing :: (A.FromJSON a) => Sing xs -> A.Value -> Parser (JSBranch xs a)
      parseSing s o = case s of
        SNil -> JSNil <$> A.parseJSON o
        x `SCons` xs -> case o of
          A.Object v -> let key = pack (reflectSym x) in
            JSCons <$> (v .: key >>= parseSing xs)
          _ -> empty


reflectSym :: SSymbol s -> String
reflectSym s = withKnownSymbol s $ proxySym s Proxy
  where
    proxySym :: (KnownSymbol n) => SSymbol n -> Proxy n -> String
    proxySym _ p = symbolVal p
