{-# OPTIONS_GHC -w -fno-warn-orphans #-}

module Network.Sieve.Test (
  addressS,
  addressL,
  addHeader,
  addHeaders,
  Config(..),
  nilMail,
  Action(..),
  assertMailActions,
  assertMailStoredIn,
  assertHeaderStoredIn,
  assertHeadersStoredIn
)
where

import           Control.Applicative ((<*), (*>), (<$>))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, ReaderT)
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

data Config = Config {
    extensions :: String
  } deriving (Show)

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

writeMailTemp :: Mail -> ReaderT Config IO FilePath
writeMailTemp mail = do
    tempDir <- liftIO getTemporaryDirectory
    pathAndHandle <- liftIO $ openBinaryTempFile tempDir "testsieve.mail"
    renderedMail <- liftIO $ renderMail' mail
    liftIO $ BSLC.hPutStr (snd pathAndHandle) renderedMail
    liftIO $ hClose $ snd pathAndHandle
    return $ fst pathAndHandle

runSieveTestWithMail :: FilePath -> Mail -> ReaderT Config IO String
runSieveTestWithMail filter mail = do
  extensions <- extensions <$> ask
  mailFile <- writeMailTemp mail
  result@(exitCode, stdout, _) <- liftIO $ readProcessWithExitCode "sieve-test" ["-x", extensions, filter, mailFile] ""
  when (exitCode /= ExitSuccess) $ liftIO $ assertFailure $ formatFailure result mailFile
  liftIO $ removeFile mailFile
  return stdout
  where
    formatFailure :: (ExitCode, String, String) -> FilePath -> String
    formatFailure (ExitFailure code, stdout, stderr) mailFile =
       "error code " ++ show code ++ " for sieve-test on '"
       ++ mailFile ++ "':\n"
       ++ "stdout:\n" ++ stdout
       ++ "\nstderr:\n" ++ stderr

data Action =
    -- actions
    Store String
  | Discard
    -- side effects
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
      (:[]) <$> choice [
          storeAction,
          discardAction
        ]
    storeAction :: Parser Action
    storeAction = Store <$> (string "store message in folder: " *> many1 (noneOf "\n"))
    discardAction :: Parser Action
    discardAction = string "discard" *> return Discard
    -- sieve_result_seffect_printf in src/lib-sieve/sieve-result.c
    someSideEffect :: Parser [Action]
    someSideEffect = do
      try $ string "       + "
      (:[]) <$> choice [
          createMailboxAction
        ]
    createMailboxAction :: Parser Action
    createMailboxAction = string "create mailbox if it does not exist" *> return CreateMailboxIfNotExist

assertMailActions :: Mail -> Actions -> ReaderT Config IO ()
assertMailActions mail expectedActions = do
    currentDir <- liftIO getCurrentDirectory
    sieveTestOut <- runSieveTestWithMail (currentDir ++ "/test.sieve") mail
    actualActions <- liftIO $ parseSieveTestOut sieveTestOut
    liftIO $ assertEqual ("unexpected Actions: " ++ sieveTestOut) expectedActions actualActions
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

assertMailStoredIn :: Mail -> String -> ReaderT Config IO ()
assertMailStoredIn mail folder = assertMailActions mail ([Store folder, CreateMailboxIfNotExist], [])

assertHeaderStoredIn :: (String, String) -> String -> ReaderT Config IO ()
assertHeaderStoredIn header = assertMailStoredIn (addHeader nilMail header)

assertHeadersStoredIn :: [(String, String)] -> String -> ReaderT Config IO ()
assertHeadersStoredIn headers = assertMailStoredIn (addHeaders nilMail headers)
