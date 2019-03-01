{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains implementation of the 'Generic' TOML codec.

module Toml.Generic
       ( genericCodec

       , GenericCodec (..)
       ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.String (IsString (..))
import Data.Type.Equality ((:~:) (..), testEquality)
import GHC.Generics ((:*:) (..), (:+:), C1, D1, Datatype (..), Generic (..), K1 (..), M1 (..), Rec0,
                     S1, Selector (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Type.Reflection (Typeable, typeRep)

import Toml.Bi (HasBiMap (..), TomlCodec, dimap, (.=))
import Toml.PrefixTree (Key)

import qualified Toml.Bi as Toml

{- | Generic codec for arbitrary data types.
-}
genericCodec :: (Generic a, GenericCodec (Rep a)) => TomlCodec a
genericCodec = dimap from to genericTomlCodec

class GenericCodec (f :: k -> Type) where
    genericTomlCodec :: TomlCodec (f p)

instance (Datatype d, GenericCodec f) => GenericCodec (D1 d f) where
    genericTomlCodec = dimap unM1 M1 genericTomlCodec

type GenericSumTomlNotSupported =
    'Text "Generic TOML deriving for arbitrary sum types is not supported currently."

instance (TypeError GenericSumTomlNotSupported) => GenericCodec (f :+: g) where
    genericTomlCodec = error "Not supported"

instance GenericCodec f => GenericCodec (C1 c f) where
    genericTomlCodec = dimap unM1 M1 genericTomlCodec

instance (GenericCodec f, GenericCodec g) => GenericCodec (f :*: g) where
    genericTomlCodec = (:*:)
        <$> genericTomlCodec .= fstG
        <*> genericTomlCodec .= sndG
      where
        fstG :: (f :*: g) p -> f p
        fstG (f :*: _) = f

        sndG :: (f :*: g) p -> g p
        sndG (_ :*: g) = g

instance (Selector s, Typeable a) => GenericCodec (S1 s (Rec0 a)) where
    genericTomlCodec = genericWrap $ codec fieldName
      where
        genericWrap :: TomlCodec a -> TomlCodec (S1 s (Rec0 a) p)
        genericWrap = dimap (unK1 . unM1) (M1 . K1)

        fieldName :: Key
        fieldName = fromString $ selName (error "S1" :: S1 s Proxy ())

        is :: forall t . Typeable t => Maybe (a :~: t)
        is = testEquality (typeRep @a) (typeRep @t)

        codec :: Key -> TomlCodec a
        codec
            | Just Refl <- is @Bool = Toml.bool
            | Just Refl <- is @Int  = Toml.int
            | otherwise = undefined

-- {- | @newtype@ for generic deriving of 'HasCodec' typeclass for custom data
-- types that should we wrapped into separate table. Use it only for data types
-- that are fields of another data types.
--
-- @
-- data Person = Person
--     { personName    :: Text
--     , personAddress :: Address
--     } deriving (Generic)
--
-- data Address = Address
--     { addressStreet :: Text
--     , addressHouse  :: Int
--     } deriving (Generic)
--       deriving HasCodec via TomlTable Address
--
-- personCodec :: TomlCodec Person
-- personCodec = genericCodec
-- @
--
-- @personCodec@ corresponds to the TOML of the following structure:
--
-- @
-- name = "foo"
-- [address]
--     street = \"Bar\"
--     house = 42
-- @
-- -}
-- newtype TomlTable a = TomlTable
--     { unTomlTable :: a
--     }
--
-- instance (Generic a, GenericCodec (Rep a)) => TomlTable a where
--     hasCodec = table genericCodec
