module FastGraph
    (
    ) where

import Data.Word

-- TODO: для начала надо реализовать логику как она есть в FastGraph,
-- потом уже думать над улучшениями.
-- Можно даже дословный перевод.

-- работа со стримом.
-- стрим умеет возвращать Strict-ByteString-и, соответсвенно мы умеем их
-- скармливать декодеру, чтобы получать результат.
{-
type NodeId   = ByteString
type EdgeTag  = Word16
type NodeAddr = Word64

type NodePredicate = (NodeId, Int) -> Bool
type EdgePredicate = EdgeTag -> Bool
type Path = [NodeId]
data StopToken = StopToken

-- | GraphBuffer должен уметь по адресу возвращать чанк и указатель на следующий
-- чанк.
data GraphBufferData m = Chunk !ByteString (m (GraphBufferData m))
                       | EndOfBuffer

data GraphBuffer = GraphBuffer



readGraphBuffer :: Get a -> GraphBuffer -> Word64 -> m a
readGraphBuffer = undefined

getId :: Get NodeId
getId = do
  idSize  <- getWord16le
  nodeId  <- getByteString $ fromIntegral idSize
  return nodeId

shortestPath :: ( Monad m )
             => GraphBuffer
             -> NodeAddr
             -> NodeAddr
             -> Int
             -> NodePredicate
             -> EdgePredicate
             -> StopToken
             -> m Path
shortestPath buffer left right maxDepth nodeFilter edgeFilter stop =
  if left == right
    then do
      onlyId <- readGraphBuffer getId buffer left
      return [onlyId]
    else undefined
-}
