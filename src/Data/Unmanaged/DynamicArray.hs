{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- | Unmanaged low overhead dynamic array
-- Dynamic array capacity is always power of 2
module Data.Unmanaged.DynamicArray
  (
   -- * Dynamic array type
    DynamicArray

   -- * Array construction and dealocation
  , empty
  , singleton
  , new
  , delete

  -- * Reading properties
  , getSize
  , sizeToCapacity

  -- * Insertion
  , add
  , read
  , unsafeRead
  , write
  , unsafeWrite
  , clean
  , unsafeClean
  , scaleUpToCapacity

  -- * Enumeration
  , foreachPtr

  -- * Conversion
  -- ** Lists
  , toList

  -- * Direct access to underlying structure
  , pokeSize
  ) where

import Data.Bits
import Data.Proxy
import Data.Unmanaged.Allocator
import Data.Unmanaged.Layout
import Foreign.Marshal.Array
import Foreign.Marshal.Utils hiding (new)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (read)

data DynamicArray a

{-# INLINE dynamicArrayAligment #-}
dynamicArrayAligment :: forall a . (Storable a) => Proxy (DynamicArray a) -> Int
dynamicArrayAligment = const $ alignment (undefined :: Int) `composeAligment` alignment (undefined :: a)

{-# INLINE headerSize #-}
headerSize :: forall a . (Storable a) => Proxy (DynamicArray a) -> Int
headerSize proxy = let al = dynamicArrayAligment proxy
                    in align al (sizeOf (undefined :: Int))

{-# INLINE itemSize #-}
itemSize :: forall a . (Storable a) => Proxy (DynamicArray a) -> Int
itemSize proxy = let al = dynamicArrayAligment proxy
                  in align al (sizeOf (undefined :: a))

{-# INLINE peekSize #-}
peekSize :: forall a . (Storable a) => Ptr (DynamicArray a) -> IO Int
peekSize ptr = peekByteOff ptr (-headerSize (Proxy :: Proxy (DynamicArray a)))

{-# INLINE pokeSize #-}
pokeSize :: forall a . (Storable a) => Ptr (DynamicArray a) -> Int -> IO ()
pokeSize ptr = pokeByteOff ptr (-headerSize (Proxy :: Proxy (DynamicArray a)))

{-# INLINE peekItem #-}
peekItem :: forall a . (Storable a) => Ptr (DynamicArray a) -> Int -> IO a
peekItem ptr i = peekByteOff ptr (itemSize (Proxy :: Proxy (DynamicArray a)) * i)

{-# INLINE pokeItem #-}
pokeItem :: forall a . (Storable a) => Ptr (DynamicArray a) -> Int -> a -> IO ()
pokeItem ptr i = pokeByteOff ptr (itemSize (Proxy :: Proxy (DynamicArray a)) * i)

-- | Create new array of specified size filled with zeros
{-# INLINE new #-}
new :: forall a . (Storable a) => Allocator -> Int -> IO (Ptr (DynamicArray a))
new Allocator {..} size = do
  let proxy = Proxy :: Proxy (DynamicArray a)
      hs = headerSize proxy
      is = itemSize proxy
      capacity = sizeToCapacity size
  basePtr <- calloc (hs + is * capacity)
  poke (castPtr basePtr) size
  return $ basePtr `plusPtr` hs

-- | Create an empty dynamic array
{-# INLINE empty #-}
empty :: forall a . (Storable a) => Allocator -> IO (Ptr (DynamicArray a))
empty Allocator {..} = do
  let hs = headerSize (Proxy :: Proxy (DynamicArray a))
  basePtr <- calloc hs
  return $ basePtr `plusPtr` hs

-- | Create dynamic array with exactly one element
{-# INLINE singleton #-}
singleton :: forall a . (Storable a) => Allocator -> a -> IO (Ptr (DynamicArray a))
singleton  Allocator {..} x = do
  let proxy = Proxy :: Proxy (DynamicArray a)
      hs = headerSize proxy
      is = itemSize proxy
  basePtr <- malloc $ hs + is
  let itemPtr = basePtr `plusPtr` hs
  poke (castPtr basePtr) (1 :: Int)
  poke itemPtr x
  return itemPtr

-- | delete GrowingVector
{-# INLINE delete #-}
delete :: forall a . (Storable a) => Allocator -> Ptr (DynamicArray a) -> IO ()
delete Allocator {..} v = free (v `plusPtr` (-headerSize (Proxy :: Proxy (DynamicArray a))))

{-# INLINE getSize #-}
getSize :: forall a . (Storable a) => Ptr (DynamicArray a) -> IO Int
getSize = peekSize

{-# INLINE sizeToCapacity #-}
sizeToCapacity :: Int -> Int
sizeToCapacity 0 = 0
sizeToCapacity size = bit $ finiteBitSize size - countLeadingZeros (size - 1)

-- | add an item to GrowingVector possibly changing GrowingVector location
{-# INLINE add #-}
add :: forall a . (Storable a) => Allocator -> Ptr (DynamicArray a) -> a -> IO (Ptr (DynamicArray a))
add Allocator {..} v x = do
  size     <- getSize v
  let capacity = sizeToCapacity size
  itemsPtr <- if size /= capacity
    then do
      pokeSize v (size + 1)
      return $ castPtr v
    else do
      let proxy = Proxy :: Proxy (DynamicArray a)
          hs = headerSize proxy
          is = itemSize proxy
          newCapactiy = if capacity /= 0 then capacity * 2 else 1
          newSize = hs + is * newCapactiy
      basePtr <- realloc (v `plusPtr` (-hs)) newSize
      poke (castPtr basePtr) (size + 1)
      return $ basePtr `plusPtr` hs
  pokeItem itemsPtr size x
  return $ castPtr itemsPtr

{-# INLINE read #-}
read :: (Storable a) => Ptr (DynamicArray a) -> Int -> IO a
read v ix = do
  size <- getSize v
  if ix < size
    then unsafeRead v ix
    else error "Data.Unmanaged.DynamicArray.read: Array index out of bounds."

{-# INLINE unsafeRead #-}
unsafeRead :: (Storable a) => Ptr (DynamicArray a) -> Int -> IO a
unsafeRead = peekItem

{-# INLINE write #-}
write :: (Storable a) => Ptr (DynamicArray a) -> Int -> a -> IO ()
write v ix x = do
  size <- getSize v
  if ix < size
    then unsafeWrite v ix x
    else error "Data.Unmanaged.DynamicArray.write: Array index out of bounds."

{-# INLINE unsafeWrite #-}
unsafeWrite :: (Storable a) => Ptr (DynamicArray a) -> Int -> a -> IO ()
unsafeWrite = pokeItem

{-# INLINE clean #-}
clean :: forall a . (Storable a) => Ptr (DynamicArray a) -> Int -> Int -> IO ()
clean ptr from to = do
  size <- getSize ptr
  if | from > to -> error "Data.Unmanaged.DynamicArray.clean: Invalid range."
     | to > size -> error "Data.Unmanaged.DynamicArray.clean: Array index out of bounds."
     | otherwise -> unsafeClean ptr from to

{-# INLINE unsafeClean #-}
unsafeClean :: forall a . (Storable a) => Ptr (DynamicArray a) -> Int -> Int -> IO ()
unsafeClean ptr from to =
  let is = itemSize (Proxy :: Proxy (DynamicArray a))
      fromAddress = ptr `plusPtr` (from * is)
      bytesCount = (to - from) * is
  in fillBytes fromAddress 0 bytesCount

{-# INLINE scaleUpToCapacity #-}
scaleUpToCapacity :: forall a . (Storable a) => Ptr (DynamicArray a) -> IO ()
scaleUpToCapacity ptr = do
  size <- getSize ptr
  let capacity = sizeToCapacity size
  if size /= capacity
    then do
      pokeSize ptr capacity
      unsafeClean ptr size capacity
    else return ()

toList :: (Storable a) => Ptr (DynamicArray a) -> IO [a]
toList v = do
  size <- getSize v
  peekArray size (castPtr v)

{-# INLINE foreachPtr #-}
foreachPtr :: forall a . (Storable a)
           => (Ptr a -> IO ()) -> Ptr (DynamicArray a) -> IO ()
foreachPtr f ptr = do
  let go 0 _ = return ()
      go c p = do
        f p
        go (c - 1) (p `plusPtr` (itemSize (Proxy :: Proxy (DynamicArray a))))
  size <- getSize ptr
  go size (castPtr ptr)
