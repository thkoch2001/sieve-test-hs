{-# OPTIONS_GHC -w -fno-warn-orphans #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import           GHC.IO.Handle
import           Network.Mail.Mime
import           System.Directory
import           System.IO.Temp

addressS :: String -> Address
addressS s = Address Nothing $ T.pack s

addressL :: String -> String -> Address
addressL s t = Address (Just $ T.pack s) $ T.pack t

addHeader :: Mail -> String -> String -> Mail
addHeader m name value = m { mailHeaders = new }
  where
    new = (mailHeaders m) ++ [(BSC.pack name, T.pack value)]

textPart :: String -> Part
textPart t = Part {
  partType = T.pack "text/plain",
  partEncoding = QuotedPrintableText,
  partFilename = Nothing,
  partHeaders = [],
  partContent = BSLC.pack t
  }

-- added no-warn-orphans for this declaration
instance Show Address where
  show (Address Nothing addr) = T.unpack addr
  show (Address (Just name) addr) = (T.unpack name) ++ " <" ++ (T.unpack addr) ++ ">"

nilMail :: Mail
nilMail = (emptyMail $ addressS "nobody@example.com") {
    mailParts = [[textPart ""]]
  }

writeMailTemp :: Mail -> IO FilePath
writeMailTemp mail = do
    tempDir <- getTemporaryDirectory
    pathAndHandle <- openBinaryTempFile tempDir "testsieve.mail"
    writeMail pathAndHandle
    return $ fst pathAndHandle
  where
    writeMail :: (FilePath, Handle) -> IO FilePath
    writeMail (path, h) = do
      renderedMail <- renderMail' mail
      BSLC.hPutStr h renderedMail
      return path

main :: IO()
main = do
  print "hi"
