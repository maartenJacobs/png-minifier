import System.IO          as IO (openBinaryFile, IOMode(ReadMode, WriteMode),
                                 hClose, Handle, withBinaryFile)
import System.Environment (getArgs)
import Data.ByteString    as B (ByteString, length, take, pack, foldl,
                                hGet, hGetContents, splitAt, empty, drop,
                                putStrLn, hPut, head)
import Data.Word          as W (Word8, Word32)
import Data.Bits (testBit)
import Data.List (findIndices)

data Chunk = Chunk { dataLength :: W.Word32
		   , chunkLength :: ByteString
                   , chunkType :: ByteString
                   , chunkData :: ByteString
                   , checkSum :: ByteString
                   } deriving (Show)

isPngFile :: B.ByteString -> Bool
isPngFile bs
  | B.length bs < 8 = False
  | otherwise = B.take 8 bs == pngSignature

pngSignature :: B.ByteString
pngSignature = B.pack [137, 80, 78, 71, 13, 10, 26, 10]

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
  | otherwise = return . Just $ if dataLength == 0
                  then emptyDataChunk
                  else Chunk dataLength lengthBytes chunkType chunkData checkSum
  where (lengthBytes, pastLength) = B.splitAt 4 bs
        dataLength = byteStringToWord32 lengthBytes
        (chunkType, pastType) = B.splitAt 4 pastLength
	emptyDataChecksum = B.take 4 pastType
        emptyDataChunk = Chunk dataLength lengthBytes chunkType B.empty emptyDataChecksum 
        (chunkData, pastData) = B.splitAt (fromIntegral dataLength) pastType
        checkSum = B.take 4 pastData

hPutPngChunks :: Handle -> [Chunk] -> IO ()
hPutPngChunks handle chunks = do hPut handle pngSignature
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

minPngFileName :: String -> String
minPngFileName filename
    | null indices = filename ++ ".min"
    | otherwise = let (name, ext) = Prelude.splitAt (last indices) filename
                  in name ++ ".min" ++ ext
    where indices = findIndices (== '.') filename

main = do 
    args <- getArgs
    handle <- IO.openBinaryFile (args !! 0) IO.ReadMode
    bytes <- B.hGetContents handle
    IO.hClose handle
    Prelude.putStrLn $ pngSigMessage bytes
    let chunksBytes = B.drop 8 bytes
    chunks <- getChunks chunksBytes
    let critChunks = filter isCriticalChunk chunks
    withBinaryFile (minPngFileName (args !! 0)) IO.WriteMode (\handle -> hPutPngChunks handle critChunks)
    return ()

