module PNG (isAncillaryChunk
          , isCriticalChunk
          , isPngFile
          , stripSignature
          , getChunks
          , hPutPngChunks
          , hPutChunks
          , hPutChunk
          ) where

import System.IO          as IO (Handle)
import Data.ByteString    as B (ByteString, length, take, pack, foldl,
                                splitAt, empty, drop,
                                hPut, head)
import Data.Word          as W (Word32)
import Data.Bits (testBit)

data Chunk = Chunk { dataLength :: Word32
		   , chunkLength :: ByteString
                   , chunkType :: ByteString
                   , chunkData :: ByteString
                   , checkSum :: ByteString
                   } deriving (Show)

isPngFile :: B.ByteString -> Bool
isPngFile bs
  | B.length bs < 8 = False
  | otherwise = B.take 8 bs == pngSignature

pngSignature :: ByteString
pngSignature = B.pack [137, 80, 78, 71, 13, 10, 26, 10]

stripSignature :: ByteString -> ByteString
stripSignature = B.drop (B.length pngSignature)

-- Most likely unnecessary: it is only here for educational purposes
byteStringToNum :: Num a => ByteString -> a
byteStringToNum bs = B.foldl addWord 0 bs
  where addWord acc w = acc * 256 + (fromIntegral w)

getChunks :: ByteString -> IO [Chunk]
getChunks bs = chunkIterator bs []
  where chunkIterator bs chunks = do nextChunk <- getChunk bs
                                     processNext bs nextChunk chunks
        processNext _ Nothing chunks = return chunks
        processNext bs (Just chunk) chunks = chunkIterator
          (B.drop (fromIntegral (dataLength chunk) + 12) bs)
          (chunks ++ [chunk])

getChunk :: ByteString -> IO (Maybe Chunk)
getChunk bs
  | B.length bs < 12 = do return Nothing
  | otherwise = return . Just $ if dataLength == 0
                  then mkEmptyChunk lengthBytes pastLength
                  else mkDataChunk dataLength lengthBytes pastLength
  where (lengthBytes, pastLength) = B.splitAt 4 bs
        dataLength = byteStringToNum lengthBytes

mkDataChunk :: Word32 -> ByteString -> ByteString -> Chunk
mkDataChunk dataLength lengthBytes bs =
    Chunk dataLength lengthBytes chunkType chunkData checkSum
    where (chunkType, pastType) = B.splitAt 4 bs
          (chunkData, pastData) = B.splitAt (fromIntegral dataLength)
                                            pastType
          checkSum = B.take 4 pastData

mkEmptyChunk :: ByteString -> ByteString -> Chunk
mkEmptyChunk lengthBytes bs = Chunk 0 lengthBytes chunkType B.empty checkSum
    where (chunkType, pastType) = B.splitAt 4 bs
          checkSum = B.take 4 pastType

hPutPngChunks :: Handle -> [Chunk] -> IO ()
hPutPngChunks handle chunks = do
    hPut handle pngSignature
    hPutChunks handle chunks
    return ()

hPutChunks :: Handle -> [Chunk] -> IO ()
hPutChunks handle = mapM_ (hPutChunk handle)

hPutChunk :: Handle -> Chunk -> IO ()
hPutChunk handle chunk = mapM_ (hPut handle) $ map ($ chunk) bsFuncs
    where bsFuncs = [chunkLength, chunkType, chunkData, checkSum]

isAncillaryChunk :: Chunk -> Bool
isAncillaryChunk = (flip testBit) 5 . B.head . chunkType

isCriticalChunk :: Chunk -> Bool
isCriticalChunk = not . isAncillaryChunk

