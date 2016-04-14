module Bench where

import Control.Monad
import Control.Monad.ST
import Criterion.Main
import qualified Data.HashTable.ST.Basic as HB
import Data.Unmanaged.Allocator
import Data.Unmanaged.ArrayHashTable ( ArrayHashTable )
import qualified Data.Unmanaged.ArrayHashTable as AH
import Foreign.Ptr

-- TODO: Bench on x-tra large tables
main :: IO ()
main = defaultMain
  [ bgroup "Unmanaged Data Structures"
    [ bgroup "Data.Unmanaged.ArrayHashTable"
      [ {- bench "empty -> delete"  $ nfIO $
          (AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))) >>= (AH.delete defaultAllocator)
      , bench "singleton -> delete" $ nfIO $
          (AH.singleton defaultAllocator (10::Int) (20::Int)) >>= (AH.delete defaultAllocator)
      , bench "singleton -> delete" $ nfIO $
          (AH.singleton defaultAllocator (10::Int) (20::Int)) >>= (AH.delete defaultAllocator)
      , -} bench "10 incremental inserts" $ nfIO $ do
          a <- AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
          b <- foldM (\t v -> AH.insert v v defaultAllocator t) a [1..10]
          AH.delete defaultAllocator b
      , bench "10 incremental inserts (Data.HashTable.ST.Basic)" $ flip nf [1..10::Int] $ \ar -> runST $ do
          a <- HB.new
          mapM_ (\k -> HB.insert a k k) ar

      , bench "100 incremental inserts" $ nfIO $ do
          a <- AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
          b <- foldM (\t v -> AH.insert v v defaultAllocator t) a [1..100]
          AH.delete defaultAllocator b
      , bench "100 incremental inserts (Data.HashTable.ST.Basic)" $ flip nf [1..100::Int] $ \ar -> runST $ do
          a <- HB.new
          mapM_ (\k -> HB.insert a k k) ar

      , bench "1000 incremental inserts" $ nfIO $ do
          a <- AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
          b <- foldM (\t v -> AH.insert v v defaultAllocator t) a [1..1000]
          AH.delete defaultAllocator b
      , bench "10000 incremental inserts (Data.HashTable.ST.Basic)" $ flip nf [1..1000::Int] $ \ar -> runST $ do
          a <- HB.new
          mapM_ (\k -> HB.insert a k k) ar

      , bench "1000 incremental inserts" $ nfIO $ do
              a <- AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
              b <- foldM (\t v -> AH.insert v v defaultAllocator t) a [1..10000]
              AH.delete defaultAllocator b
      , bench "10000 incremental inserts (Data.HashTable.ST.Basic)" $ flip nf [1..10000::Int] $ \ar -> runST $ do
          a <- HB.new
          mapM_ (\k -> HB.insert a k k) ar
      ]
    ]
  ]
