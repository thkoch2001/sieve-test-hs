{-# OPTIONS_GHC -w -fno-warn-orphans #-}

import           Control.Applicative ((<*), (*>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import           Network.Mail.Mime
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO.Temp (openBinaryTempFile)
import           System.Process (readProcessWithExitCode)
import           Text.Parsec (parse)
import           Text.Parsec.Char (string, anyChar, char, noneOf)
import           Text.Parsec.Combinator (many1, between, choice)
import           Text.Parsec.Prim ((<|>))
import           Text.Parsec.String (Parser)

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
    renderedMail <- renderMail' mail
    BSLC.hPutStr (snd pathAndHandle) renderedMail
    return $ fst pathAndHandle

runSieveTest :: FilePath -> FilePath -> IO String
runSieveTest filter mail = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "sieve-test" args ""
    return stdout
  where
    args :: [String]
    args = [filter, mail]

runSieveTestWithMail :: FilePath -> Mail -> IO String
runSieveTestWithMail filter mail = do
  mailFile <- writeMailTemp mail
  stdout <- runSieveTest filter mailFile
  removeFile mailFile
  return stdout

out :: String
out = "\nPerformed actions:\n\n  (none)\n\nImplicit keep:\n\n * store message in folder: INBOX\n\n"

data Action =
  Store String
  deriving (Show, Eq, Ord)

parseSieveTestResult :: Parser ([Action], [Action])
parseSieveTestResult = do
    string "\nPerformed actions:\n\n"
    performedActions <- actionLines
    string "\nImplicit keep:\n\n"
    implicitKeep <- actionLines
    char '\n'
    return $ (performedActions, implicitKeep)
  where
    actionLines :: Parser [Action]
    actionLines = fmap concat $ many1 actionLine
    actionLine :: Parser [Action]
    actionLine = char ' ' *> action <* char '\n'
    action = none <|> someAction
    none :: Parser [Action]
    none = do
      string " (none)"
      return []
    someAction :: Parser [Action]
    someAction = do
      string "* "
      action <- choice [
          storeAction
        ]
      return [action]
    storeAction :: Parser Action
    storeAction = do
      folder <- (string "store message in folder: ") *> (many1 $ noneOf "\n")
      return $ Store folder


main :: IO()
main = do
  print "hi"
