module Data.Unmanaged.Allocator
  ( PVoid
  , Allocator (..)
  , defaultAllocator
  ) where

import Data.Void
import Foreign.Ptr
import qualified Foreign.Marshal.Alloc as F

type PVoid = Ptr Void

data Allocator = Allocator
  { malloc  :: !(Int -> IO PVoid)
  , calloc  :: !(Int -> IO PVoid)
  , realloc :: !(PVoid -> Int -> IO PVoid)
  , free    :: !(PVoid -> IO ())
  }

-- TODO: добавить счётчик статистики
-- ТODO: Возможно следует завести монадку для аллокаторов, но не следует с
-- этим торопиться.
defaultAllocator :: Allocator
defaultAllocator = Allocator
  { malloc = \size -> do
      -- putStrLn $ "malloc size = " ++ (show size)
      res <- F.mallocBytes size
      -- putStrLn $ "res = " ++ (show res)
      return res
  , calloc = \size -> do
      -- putStrLn $ "calloc size = " ++ (show size)
      res <- F.callocBytes size
      -- putStrLn $ "res = " ++ (show res)
      return res
  , realloc = \ptr size -> do
      -- putStrLn $ "realloc ptr = " ++ (show ptr) ++ " size = " ++ (show size)
      res <- F.reallocBytes ptr size
      -- putStrLn $ "res = " ++ (show res)
      return res
  , free = \ptr -> do
      -- putStrLn $ "free ptr = " ++ (show ptr)
      F.free ptr
  }
