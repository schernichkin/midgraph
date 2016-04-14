module Main where

import Data.Unmanaged.Allocator
import Data.Unmanaged.ArrayHashTable as AH
import Control.Monad
import Foreign.Ptr

main :: IO ()
main = replicateM_ 1000 $ do
  putStrLn "start"
  a <- AH.empty defaultAllocator :: IO (Ptr (ArrayHashTable Int Int))
  b <- foldM (\t v -> AH.insert v v defaultAllocator t) a [1..10]
  AH.delete defaultAllocator b
  putStrLn "end"
  return ()
