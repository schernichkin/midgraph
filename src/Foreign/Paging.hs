{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Foreign.Paging where


import Foreign.Ptr
import Prelude hiding (lookup)

data MappedPage = MappedPage
  { _pageId    :: Int
  , _rawAddr   :: Ptr ()
  , _rawSize   :: Int
  , _addr      :: Ptr ()
  }



{-
-- TODO: 1. кешь страниц. кешь страниц может захотеть добавлять свои данные
-- непосредственно в страницу.

-- Simple page manager which allocates pages in memory and deallocates it
-- as soon as page no longer needed.
data MemoryPageManager = MallocPageManager
  { _pageSize :: Int
  , _pages    :: IntMap (Ptr Word8)
  }

data MemoryPage = MemoryPage
  { _pageId   :: Int
  , _pageAddr :: Ptr Word8
  }

aquireMemoryPage :: MemoryPageManager -> Int -> IO (MemoryPage, MemoryPageManager)
aquireMemoryPage pm@MallocPageManager {..} i = do
  case lookup i _pages of
    Just addr -> return $ (MemoryPage i addr, pm)
    Nothing       -> do
      addr <- mallocBytes _pageSize
      return ( MemoryPage i addr, pm { _pages = insert i addr _pages } )

releaseMemoryPage :: MemoryPageManager -> MemoryPage -> IO MemoryPageManager
releaseMemoryPage pm@MallocPageManager {..} p@MemoryPage {..} = do
  undefined
-}
