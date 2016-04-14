{-# LANGUAGE ScopedTypeVariables #-}

module Data.Unmanaged.KeyValuePair
    ( -- * KeyValuePair type
      KeyValuePair (..)

      -- * Direct access to underlying structure
    , peekKey
    , peekValue
    , pokeKey
    , pokeValue

    -- * Layout information
    , keyValuePairSize
    , keyValuePairAlignment
    , valueOffset
    ) where

import Data.Proxy
import Data.Unmanaged.Layout
import Foreign.Ptr
import Foreign.Storable

data KeyValuePair k v = KeyValuePair
  { _key   :: !k
  , _value :: !v
  }

instance ( Storable k, Storable v ) => Storable (KeyValuePair k v) where
  sizeOf = const $ keyValuePairSize (Proxy :: Proxy (KeyValuePair k v))
  alignment = const $ keyValuePairAlignment (Proxy :: Proxy (KeyValuePair k v))
  peek ptr = KeyValuePair <$> peekKey ptr
                          <*> peekValue ptr
  poke ptr (KeyValuePair k v) = pokeKey ptr k >> pokeValue ptr v

{-# INLINE keyValuePairAlignment #-}
keyValuePairAlignment :: forall k v . ( Storable k, Storable v )
                      => Proxy (KeyValuePair k v) -> Int
keyValuePairAlignment = const $ alignment (undefined :: k) `composeAligment` alignment (undefined :: v)

{-# INLINE valueOffset #-}
valueOffset :: forall k v . ( Storable k, Storable v ) => Proxy (KeyValuePair k v) -> Int
valueOffset proxy = let al = keyValuePairAlignment proxy
                    in align al (sizeOf (undefined :: k))

{-# INLINE keyValuePairSize #-}
keyValuePairSize :: forall k v . ( Storable k, Storable v )
                 => Proxy (KeyValuePair k v) -> Int
keyValuePairSize proxy = let al = keyValuePairAlignment proxy
                         in valueOffset proxy
                          + align al (sizeOf (undefined::v))

{-# INLINE peekKey #-}
peekKey :: forall k v . ( Storable k, Storable v ) => Ptr (KeyValuePair k v) -> IO k
peekKey = peek . castPtr

{-# INLINE peekValue #-}
peekValue :: forall k v . ( Storable k, Storable v ) => Ptr (KeyValuePair k v) -> IO v
peekValue ptr = peekByteOff ptr (valueOffset (Proxy :: Proxy (KeyValuePair k v)))

{-# INLINE pokeKey #-}
pokeKey :: forall k v . ( Storable k, Storable v ) => Ptr (KeyValuePair k v) -> k -> IO ()
pokeKey = poke . castPtr

{-# INLINE pokeValue #-}
pokeValue :: forall k v . ( Storable k, Storable v ) => Ptr (KeyValuePair k v) -> v -> IO ()
pokeValue ptr = pokeByteOff ptr (valueOffset (Proxy :: Proxy (KeyValuePair k v)))
