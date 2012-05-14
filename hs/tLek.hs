import System.Environment
import System.IO hiding (putStrLn)
import Crypto.Hash.SHA1
import Data.ByteString.Lazy as LZ
import Data.ByteString.Lazy.Internal
import Data.Hex

main = do
    args <- getArgs
    let fname = args!!0
    content <- LZ.readFile fname
    -- to print binary:
    -- LZ.putStrLn $ LZ.fromChunks [hashlazy content]
    -- to print text:
    LZ.putStrLn $ hex (LZ.fromChunks [hashlazy content])
