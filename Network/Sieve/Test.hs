{-# OPTIONS_GHC -w -fno-warn-orphans #-}

module Network.Sieve.Test (
  addressS,
  addressL,
  addHeader,
  nilMail,
  Action(..),
  assertMailActions,
  assertMailStoredIn,
  assertHeaderStoredIn,
  assertHeadersStoredIn
)
where

import           Control.Applicative ((<*), (*>))
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BSC (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, hPutStr)
import qualified Data.Text as T (pack, Text, unpack)
import           GHC.IO.Exception (ExitCode(..))
import           Network.Mail.Mime (Address(Address), emptyMail, Mail(..), Part(..), Encoding(..), renderMail')
import           System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import           System.IO (hClose)
import           System.IO.Temp (openBinaryTempFile)
import           System.Process (readProcessWithExitCode)
import           Test.HUnit.Base (assertFailure, assertEqual)
import           Text.Parsec (parse)
import           Text.Parsec.Char (string, char, noneOf)
import           Text.Parsec.Combinator (many1, choice)
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.Prim (try)
import           Text.Parsec.String (Parser)

addressS :: String -> Address
addressS s = Address Nothing $ T.pack s

addressL :: String -> String -> Address
addressL s t = Address (Just $ T.pack s) $ T.pack t

type Header = (BSC.ByteString, T.Text)

packHeader :: (String, String) -> Header
packHeader (name, value) = (BSC.pack name, T.pack value)

addHeader :: Mail -> (String, String) -> Mail
addHeader mail header = addHeaders mail [header]

addHeaders :: Mail -> [(String, String)] -> Mail
addHeaders mail headers = mail { mailHeaders = new }
  where
    new :: [Header]
    new = mailHeaders mail ++ fmap packHeader headers

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
  show (Address (Just name) addr) = T.unpack name ++ " <" ++ T.unpack addr ++ ">"

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
    hClose $ snd pathAndHandle
    return $ fst pathAndHandle

runSieveTestWithMail :: FilePath -> Mail -> IO String
runSieveTestWithMail filter mail = do
  mailFile <- writeMailTemp mail
  result@(exitCode, stdout, _) <- readProcessWithExitCode "sieve-test" ["-x", "regex variables fileinto envelope mailbox", filter, mailFile] ""
  when (exitCode /= ExitSuccess) $ assertFailure $ formatFailure result mailFile
  removeFile mailFile
  return stdout
  where
    formatFailure :: (ExitCode, String, String) -> FilePath -> String
    formatFailure (ExitFailure code, stdout, stderr) mailFile =
       "error code " ++ show code ++ " for sieve-test on '"
       ++ mailFile ++ "':\n"
       ++ "stdout:\n" ++ stdout
       ++ "\nstderr:\n" ++ stderr

data Action =
    Store String
  | CreateMailboxIfNotExist
  deriving (Show, Eq, Ord)

type Actions = ([Action], [Action])

parseSieveTestResult :: Parser Actions
parseSieveTestResult = do
    string "\nPerformed actions:\n\n"
    performedActions <- actionLines
    string "\nImplicit keep:\n\n"
    implicitKeep <- actionLines
    char '\n'
    return (performedActions, implicitKeep)
  where
    actionLines :: Parser [Action]
    actionLines = fmap concat $ many1 actionLine
    actionLine :: Parser [Action]
    actionLine = char ' ' *> action <* char '\n'
    action :: Parser [Action]
    action = choice [none, someAction, someSideEffect]
    none :: Parser [Action]
    none = do
      try $ string " (none)"
      return []
    -- sieve_result_action_printf in src/lib-sieve/sieve-result.c
    someAction :: Parser [Action]
    someAction = do
      try $ string "* "
      action <- choice [
          storeAction
        ]
      return [action]
    storeAction :: Parser Action
    storeAction = do
      folder <- string "store message in folder: " *> many1 (noneOf "\n")
      return $ Store folder
    -- sieve_result_seffect_printf in src/lib-sieve/sieve-result.c
    someSideEffect :: Parser [Action]
    someSideEffect = do
      try $ string "       + "
      action <- choice [
          createMailboxAction
        ]
      return [action]
    createMailboxAction :: Parser Action
    createMailboxAction = do
      string "create mailbox if it does not exist"
      return CreateMailboxIfNotExist

assertMailActions :: Mail -> Actions -> IO ()
assertMailActions mail expectedActions = do
    currentDir <- getCurrentDirectory
    sieveTestOut <- runSieveTestWithMail (currentDir ++ "/test.sieve") mail
    actualActions <- parseSieveTestOut sieveTestOut
    assertEqual ("unexpected Actions: " ++ sieveTestOut) expectedActions actualActions
    return ()
  where
    parseSieveTestOut :: String -> IO Actions
    parseSieveTestOut s = case parse parseSieveTestResult "" s of
      (Left error) -> do
        assertFailure $ "could not parse output from sieve-test:\n"
          ++ show error
          ++ "output was:\n"
          ++ s
        return ([], [])
      (Right actions) -> return actions

assertMailStoredIn :: Mail -> String -> IO ()
assertMailStoredIn mail folder = assertMailActions mail ([Store folder, CreateMailboxIfNotExist], [])

assertHeaderStoredIn :: (String, String) -> String -> IO ()
assertHeaderStoredIn header = assertMailStoredIn (addHeader nilMail header)

assertHeadersStoredIn :: [(String, String)] -> String -> IO ()
assertHeadersStoredIn headers = assertMailStoredIn (addHeaders nilMail headers)
