import System.IO          as IO (IOMode(ReadMode, WriteMode),
                                 withBinaryFile)
import System.Environment (getArgs)
import Data.ByteString    as B (ByteString
                              , hGetContents)
import Data.List (findIndices)

import PNG (stripSignature, getChunks, isCriticalChunk, hPutPngChunks,
            isPngFile)

minPngFileName :: String -> String
minPngFileName filename
    | null indices = filename ++ ".min"
    | otherwise = let (name, ext) = Prelude.splitAt (last indices) filename
                  in name ++ ".min" ++ ext
    where indices = findIndices (== '.') filename

minifyPng :: String -> ByteString -> IO ()
minifyPng filename bytes = do
    let chunksBytes = stripSignature bytes
    chunks <- getChunks chunksBytes
    let critChunks = filter isCriticalChunk chunks
    let outputFilename = minPngFileName filename
    let outputHandler = (flip hPutPngChunks) critChunks
    withBinaryFile outputFilename WriteMode outputHandler
    return ()

main :: IO ()
main = do
    args <- getArgs
    let filename = args !! 0
    bytes <- withBinaryFile filename ReadMode hGetContents
    if not (isPngFile bytes)
        then Prelude.putStrLn "Unknown file type"
        else minifyPng filename bytes

