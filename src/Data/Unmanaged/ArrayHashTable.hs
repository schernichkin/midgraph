{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Unmanaged.ArrayHashTable
  (
  -- * Hask table type
    ArrayHashTable

  -- * Construction and deallocation
  , empty
  , singleton
  , delete

  -- * Query
  , lookupPtr

  -- * Insertion
  , insert

  -- * Conversion
  -- ** Lists
  , toList

  -- * TODO: remove
  , test
  ) where

import Data.Bits
import Data.Hashable
import Data.Proxy
import Data.Unmanaged.Allocator
import Data.Unmanaged.DynamicArray ( DynamicArray )
import qualified Data.Unmanaged.DynamicArray as DA
import Data.Unmanaged.KeyValuePair ( KeyValuePair (..) )
import qualified Data.Unmanaged.KeyValuePair as KV
import Data.Unmanaged.Layout
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding ( lookup )

-- | Pointer tag for overflow entries created as result of hash collision.
type Overflow k v = DynamicArray (KeyValuePair k v)

data Bucket k v = Bucket
  { _overflow :: !(Ptr (Overflow k v))
  -- ^ pointer to collision array, or pointer to this cell if  no collision
  -- or null if cell unoccupied
  , _key      :: !k
  , _value    :: !v
  }

instance ( Storable k, Storable v ) => Storable (Bucket k v) where
  sizeOf = const $ sizeOfbucket (Proxy :: Proxy (Bucket k v))
  alignment = const $ bucketAlignment (Proxy :: Proxy (Bucket k v))
  peek ptr = Bucket <$> peekOverflow ptr <*> peekKey ptr <*> peekValue ptr
  poke ptr (Bucket o k v) = do
    pokeOverflow ptr o
    pokeKey ptr k
    pokeValue ptr v

{-# INLINE bucketAlignment #-}
bucketAlignment :: forall k v . ( Storable k, Storable v )
                  => Proxy (Bucket k v) -> Int
bucketAlignment = const $ alignment (undefined :: k) `composeAligment`
                            alignment (undefined :: v) `composeAligment`
                            alignment (undefined :: Ptr (Overflow k v))

{-# INLINE keyOffset #-}
keyOffset :: forall k v . ( Storable k, Storable v )
          => Proxy (Bucket k v) -> Int
keyOffset proxy = let al = bucketAlignment proxy
                  in align al (sizeOf (undefined :: (Ptr (Overflow k v))))

{-# INLINE valueOffset #-}
valueOffset :: forall k v . ( Storable k, Storable v )
            => Proxy (Bucket k v) -> Int
valueOffset proxy = let al = bucketAlignment proxy
                    in align al (keyOffset proxy + sizeOf (undefined :: k))

{-# INLINE sizeOfbucket #-}
sizeOfbucket :: forall k v . ( Storable k, Storable v )
             => Proxy (Bucket k v) -> Int
sizeOfbucket proxy = let al = bucketAlignment proxy
                     in align al (valueOffset proxy + sizeOf (undefined :: v))

{-# INLINE peekOverflow #-}
peekOverflow :: forall k v . ( Storable k, Storable v )
             => Ptr (Bucket k v) -> IO (Ptr (Overflow k v))
peekOverflow = peek . castPtr

{-# INLINE peekKey #-}
peekKey :: forall k v . ( Storable k, Storable v )
        => Ptr (Bucket k v) -> IO k
peekKey ptr = peekByteOff ptr (keyOffset (Proxy :: Proxy (Bucket k v)))

{-# INLINE peekValue #-}
peekValue :: forall k v . ( Storable k, Storable v )
        => Ptr (Bucket k v) -> IO v
peekValue ptr = peekByteOff ptr (valueOffset (Proxy :: Proxy (Bucket k v)))

{-# INLINE pokeOverflow #-}
pokeOverflow :: forall k v . ( Storable k, Storable v )
             => Ptr (Bucket k v) -> Ptr (Overflow k v) -> IO ()
pokeOverflow = poke . castPtr

{-# INLINE pokeKey #-}
pokeKey :: forall k v . ( Storable k, Storable v )
        => Ptr (Bucket k v) -> k -> IO ()
pokeKey ptr = pokeByteOff ptr (keyOffset (Proxy :: Proxy (Bucket k v)))

{-# INLINE pokeValue #-}
pokeValue :: forall k v . ( Storable k, Storable v )
          => Ptr (Bucket k v) -> v -> IO ()
pokeValue ptr = pokeByteOff ptr (valueOffset (Proxy :: Proxy (Bucket k v)))

data BucketOverflow k v = BucketEmpty
                        | BucketSingle
                        | BucketOverflow !(Ptr (Overflow k v))

{-# INLINE getOverflow #-}
getOverflow :: forall k v . ( Storable k, Storable v )
            => Ptr (Bucket k v) -> IO (BucketOverflow k v)
getOverflow bucketPtr = do
  overflowPtr <- peekOverflow bucketPtr
  -- putStrLn $ "getOverflow: bucketPtr = " ++ (show bucketPtr) ++ " overflowPtr = " ++ (show overflowPtr)
  return $ if | overflowPtr == nullPtr -> BucketEmpty
              | overflowPtr == castPtr bucketPtr -> BucketSingle
              | otherwise -> BucketOverflow overflowPtr

-- | Array hash table pointer tag
data ArrayHashTable k v

{-# INLINE empty #-}
empty :: forall k v . ( Storable k, Storable v )
      => Allocator -> IO (Ptr (ArrayHashTable k v))
empty allocator = do
  buckets :: Ptr (DynamicArray (Bucket k v)) <- DA.empty allocator
  return $ castPtr buckets

{-# INLINE singleton #-}
singleton :: forall k v . ( Storable k, Storable v )
          => Allocator -> k -> v -> IO (Ptr (ArrayHashTable k v))
singleton allocator key value = do
  buckets <- DA.singleton allocator $ Bucket nullPtr key value
  pokeOverflow (castPtr buckets :: Ptr (Bucket k v)) (castPtr buckets)
  return $ castPtr buckets

{-# INLINE delete #-}
delete :: forall k v . ( Storable k, Storable v )
       => Allocator -> Ptr (ArrayHashTable k v) -> IO ()
delete allocator ptr = do
  DA.foreachPtr deleteOverfow bucketArray
  -- putStrLn $ "delete: delete bucketArray at " ++ (show bucketArray)
  DA.delete allocator bucketArray
  where
    bucketArray = castPtr ptr :: Ptr (DynamicArray (Bucket k v))
    deleteOverfow bucketPtr = do
      overflow <- getOverflow bucketPtr
      case overflow of
        BucketOverflow overflowPtr -> do
          -- putStrLn $ "delete: delete overflow at " ++ (show overflowPtr) ++ " for bucket = " ++ (show bucketPtr)
          DA.delete allocator overflowPtr
        _ -> return ()

data Lookup k v = Found !(Found k v)
                | NotFound !Int !Int !(NotFound k v)

data Found k v = FoundBucket !(Ptr (Bucket k v))
               | FoundOverfow !(Ptr (KeyValuePair k v))

data NotFound k v = NotFoundEmptyBucket !(Ptr (Bucket k v))
                  | NotFoundSingleBucket !(Ptr (Bucket k v))
                  | NotFoundOverfow !(Ptr (Bucket k v)) !(Ptr (Overflow k v))

-- | Calcucale bucket ptr for specified key and capacity.
-- Capacity must be power of 2
{-# INLINE bucketPtrFor #-}
bucketPtrFor :: forall k v . ( Storable k, Hashable k, Storable v )
             => Ptr (ArrayHashTable k v) -> k -> Int -> Ptr (Bucket k v)
bucketPtrFor basePtr key capacity = plusPtr basePtr $ bucketSize * (hash key .&. (capacity - 1))
  where
    bucketSize = sizeOfbucket (Proxy :: Proxy (Bucket k v))

{-# INLINE lookup' #-}
lookup' :: forall k v . ( Storable k, Hashable k, Eq k, Storable v )
        => k
        -> Ptr (ArrayHashTable k v)
        -> IO (Lookup k v)
lookup' key ptr = do
  size <- DA.getSize (castPtr ptr :: Ptr (DynamicArray (Bucket k v)))
  if size == 0
    then return $ NotFound 0 0 $ NotFoundEmptyBucket (castPtr ptr)
    else do
      let capacity = DA.sizeToCapacity size
          bucketPtr = bucketPtrFor ptr key capacity
      overflow <- getOverflow bucketPtr
      case overflow of
        BucketSingle -> do
          bucketKey <- peekKey bucketPtr
          return $ if bucketKey == key
            then Found $ FoundBucket bucketPtr
            else NotFound size capacity $ NotFoundSingleBucket bucketPtr
        BucketEmpty -> do
          return $ NotFound size capacity $ NotFoundEmptyBucket bucketPtr
        BucketOverflow overflowPtr -> do
          bucketKey <- peekKey bucketPtr
          if bucketKey == key
            then return $ Found $ FoundBucket bucketPtr
            else do
              let scanOverflow 0 _ = return $ NotFound size capacity $ NotFoundOverfow bucketPtr overflowPtr
                  scanOverflow c entryPtr = do
                    overflowKey <- KV.peekKey entryPtr
                    if (overflowKey == key)
                      then return $ Found $ FoundOverfow entryPtr
                      else scanOverflow (c - 1) $ entryPtr `plusPtr` (KV.keyValuePairSize (Proxy :: Proxy (KeyValuePair k v)))
              overflowSize <- DA.getSize overflowPtr
              scanOverflow overflowSize (castPtr overflowPtr :: Ptr (KeyValuePair k v))

{-# INLINE lookupPtr #-}
lookupPtr :: forall k v . ( Storable k, Hashable k, Eq k, Storable v )
       => k -> Ptr (ArrayHashTable k v) -> IO (Ptr v)
lookupPtr key ptr = do
  res <- lookup' key ptr
  return $ case res of
    Found found -> case found of
       FoundBucket bucketPtr -> bucketPtr `plusPtr` (valueOffset (Proxy :: Proxy (Bucket k v)))
       FoundOverfow entryPtr -> entryPtr `plusPtr` (KV.valueOffset (Proxy :: Proxy (KeyValuePair k v)))
    NotFound _ _ _ -> nullPtr

{-# INLINE insert #-}
insert :: forall k v . ( Storable k, Hashable k, Eq k, Storable v )
       => k
       -> v
       -> Allocator
       -> Ptr (ArrayHashTable k v)
       -> IO (Ptr (ArrayHashTable k v))
insert key value allocator ptr  = do
  -- putStrLn $ "ArrayHashTable.insert ptr = " ++ (show ptr)
  lookupResult <- lookup' key ptr
  case lookupResult of
    Found found -> do
      case found of
        FoundBucket bucketPtr -> do
          -- putStrLn $ "ArrayHashTable.insert FoundBucket at = " ++ (show bucketPtr)
          pokeValue bucketPtr value -- no need to update overflow pointer
        FoundOverfow entryPtr -> do
          -- putStrLn $ "FoundOverfow"
          KV.pokeValue entryPtr value
      -- putStrLn $ "ArrayHashTable.insert: done with same array: ptr = " ++ (show ptr)
      return ptr
    NotFound size capacity notFound | size /= capacity -> do
      -- putStrLn $ "NotFound size /= capacity: size = " ++ (show size) ++ "capacity = " ++ (show capacity)
      case notFound of
        NotFoundEmptyBucket bucketPtr ->
          poke bucketPtr $ Bucket (castPtr bucketPtr) key value
        NotFoundSingleBucket bucketPtr -> do
          overflowPtr <- DA.singleton allocator $ KeyValuePair key value
          pokeOverflow bucketPtr overflowPtr
        NotFoundOverfow bucketPtr overflowPtr -> do
          overflowPtr' <- DA.add allocator overflowPtr $ KeyValuePair key value
          pokeOverflow bucketPtr overflowPtr'
      DA.pokeSize bucketArray (size + 1)
      -- putStrLn $ "ArrayHashTable.insert: done with same array: ptr = " ++ (show ptr)
      return ptr
    NotFound size capacity _ -> do
      -- putStrLn $ "ArrayHashTable.insert: NotFound size /= capacity: size = " ++ (show size) ++ "capacity = " ++ (show capacity)
      let newCapacity = if capacity == 0 then 1 else capacity * 2
      -- putStrLn $ "ArrayHashTable.insert: new capacity will be " ++ (show newCapacity)
      newBucketArray :: Ptr (ArrayHashTable k v) <- castPtr <$> (DA.new allocator newCapacity :: IO (Ptr (DynamicArray (Bucket k v))))
      -- putStrLn $ "ArrayHashTable.insert: new bucket array allocated at = " ++ (show newBucketArray)
      let insertToNewTable k v = do
            let bucketPtr = bucketPtrFor newBucketArray k newCapacity
            o <- getOverflow bucketPtr
            case o of
              BucketEmpty -> do
                -- putStrLn $ "insert: NotFound: BucketEmpty: bucketPtr = " ++ (show bucketPtr)
                poke bucketPtr $ Bucket (castPtr bucketPtr) k v
              BucketSingle -> do
                newOverflow <- DA.singleton allocator $ KeyValuePair k v
                pokeOverflow bucketPtr newOverflow
              BucketOverflow overfowPtr -> do
                newOverflow <- DA.add allocator overfowPtr $ KeyValuePair k v
                pokeOverflow bucketPtr newOverflow
          copyFromOverflow :: Ptr (KeyValuePair k v) -> IO ()
          copyFromOverflow entryPtr = do
            k <- KV.peekKey entryPtr
            v <- KV.peekValue entryPtr
            insertToNewTable k v
          processBucket :: Ptr (Bucket k v) -> IO ()
          processBucket bucketPtr = do
            overflow <- getOverflow bucketPtr
            case overflow of
              BucketSingle -> do
                k <- peekKey bucketPtr
                v <- peekValue bucketPtr
                insertToNewTable k v
              BucketEmpty -> return ()
              BucketOverflow o -> do
                k <- peekKey bucketPtr
                v <- peekValue bucketPtr
                insertToNewTable k v
                DA.foreachPtr copyFromOverflow o
                -- putStrLn $ "ArrayHashTable.insert: delete overflow at " ++ (show o)
                DA.delete allocator o
      DA.foreachPtr processBucket bucketArray
      -- putStrLn $ "ArrayHashTable.insert: delete bucketArray at " ++ (show bucketArray)
      DA.delete allocator bucketArray
      insertToNewTable key value
      DA.pokeSize (castPtr newBucketArray :: Ptr (DynamicArray (Bucket k v))) (size + 1)
      -- putStrLn $ "ArrayHashTable.insert: done with new array: ptr = " ++ (show newBucketArray)
      return newBucketArray
  where
    bucketArray = castPtr ptr :: Ptr (DynamicArray (Bucket k v))

toList :: forall k v . ( Storable k, Storable v )
       => Ptr (ArrayHashTable k v) -> IO [(k, v)]
toList ptr = do
  let readHashCells 0 acc _ = return acc
      readHashCells c acc cellPtr = do
        let cellSize = sizeOfbucket (Proxy :: Proxy (Bucket k v))
            readSingleCell = (,) <$> peekKey cellPtr <*> peekValue cellPtr
            continue a = readHashCells (c - 1) a (cellPtr `plusPtr` cellSize)
        overflow <- getOverflow cellPtr
        case overflow of
          BucketSingle -> readSingleCell >>= continue . (flip (:) acc)
          BucketEmpty  -> continue acc
          BucketOverflow overPtr -> do
            cell <- readSingleCell
            overflowList <- map (\(KeyValuePair k v) -> (k, v)) <$> DA.toList overPtr
            continue $ overflowList ++ (cell : acc)
  size <- DA.getSize (castPtr ptr :: Ptr (DynamicArray (Bucket k v)))
  let capacity = DA.sizeToCapacity size
  readHashCells capacity [] (castPtr ptr :: Ptr (Bucket k v))
{-
test = do
  a <- empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
  b <- foldM (\t v -> insert v v defaultAllocator t) a [1..1000]
  -- AH.delete defaultAllocator b
  lst <- toList b
  delete defaultAllocator b
  return lst
-}

test = do
  a <- empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
  b <- foldM (\t v -> insert v v defaultAllocator t) a [1..10]
  delete defaultAllocator b
