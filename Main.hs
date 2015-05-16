import System.IO          as IO (openBinaryFile, IOMode(ReadMode), hClose, 
                                 Handle)
import System.Environment (getArgs)
import Data.ByteString    as B (ByteString, length, take, pack, foldl,
                                hGet, hGetContents, splitAt, empty, drop,
                                putStrLn)
import Data.Word          as W (Word8, Word32)

data Chunk = Chunk { dataLength :: W.Word32
                   , chunkType :: ByteString
                   , chunkData :: ByteString
                   , checkSum :: ByteString
                   } deriving (Show)

isPngFile :: B.ByteString -> Bool
isPngFile bs
  | B.length bs < 8 = False
  | otherwise = B.take 8 bs == pngSignature
  where pngSignature = B.pack [137, 80, 78, 71, 13, 10, 26, 10]

pngSigMessage :: B.ByteString -> String
pngSigMessage bs
  | isPngFile bs = "PNG file"
  | otherwise = "Unknown file type"


-- Most likely unnecessary: it is only here for educational purposes
byteStringToWord32 :: B.ByteString -> W.Word32
byteStringToWord32 bs = B.foldl addWord 0 bs
  where addWord :: W.Word32 -> W.Word8 -> W.Word32
        addWord acc w = acc * 256 + (fromIntegral w)

getChunks :: B.ByteString -> IO [Chunk]
getChunks bs = chunkIterator bs []
  where chunkIterator bs chunks = do nextChunk <- getChunk bs
                                     processNext bs nextChunk chunks
        processNext _ Nothing chunks = return chunks
        processNext bs (Just chunk) chunks = chunkIterator 
          (B.drop (fromIntegral (dataLength chunk) + 12) bs) 
          (chunks ++ [chunk])

getChunk :: B.ByteString -> IO (Maybe Chunk)
getChunk bs
  | B.length bs < 12 = do return Nothing
  | otherwise = return $ if dataLength == 0
                  then Just emptyDataChunk
                  else Just $ Chunk dataLength chunkType chunkData checkSum 
  where (lengthBytes, pastLength) = B.splitAt 4 bs
        dataLength = byteStringToWord32 lengthBytes
        (chunkType, pastType) = B.splitAt 4 pastLength
        emptyDataChunk = Chunk dataLength chunkType B.empty (B.take 4 pastType)
        (chunkData, pastData) = B.splitAt (fromIntegral dataLength) pastType
        checkSum = B.take 4 pastData

main = do args <- getArgs
          handle <- IO.openBinaryFile (args !! 0) IO.ReadMode
          bytes <- B.hGetContents handle
          IO.hClose handle
          
          Prelude.putStrLn $ pngSigMessage bytes
          let chunksBytes = B.drop 8 bytes
          chunks <- getChunks chunksBytes
          mapM_ (B.putStrLn . chunkType) chunks
          return ()

