module Rom where
import qualified Data.ByteString as B

readRom :: FilePath -> IO ()
readRom fileName = do
  file <- B.readFile fileName   
  let bits = B.unpack file
  print bits
