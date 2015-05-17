import System.IO          as IO (IOMode(ReadMode, WriteMode),
                                 Handle, withBinaryFile)
import System.Environment (getArgs)
import Data.ByteString    as B (ByteString, length, take, pack, foldl,
                                hGetContents, splitAt, empty, drop,
                                hPut, head)
import Data.Word          as W (Word32)
import Data.Bits (testBit)
import Data.List (findIndices)

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

pngSigMessage :: ByteString -> String
pngSigMessage bs
  | isPngFile bs = "PNG file"
  | otherwise = "Unknown file type"

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

minPngFileName :: String -> String
minPngFileName filename
    | null indices = filename ++ ".min"
    | otherwise = let (name, ext) = Prelude.splitAt (last indices) filename
                  in name ++ ".min" ++ ext
    where indices = findIndices (== '.') filename

main :: IO ()
main = do
    args <- getArgs
    let filename = args !! 0
    bytes <- withBinaryFile filename ReadMode hGetContents
    Prelude.putStrLn $ pngSigMessage bytes
    let chunksBytes = B.drop 8 bytes
    chunks <- getChunks chunksBytes
    let critChunks = filter isCriticalChunk chunks
    let outputFilename = minPngFileName filename
    let outputHandler = (flip hPutPngChunks) critChunks
    withBinaryFile outputFilename WriteMode outputHandler
    return ()

