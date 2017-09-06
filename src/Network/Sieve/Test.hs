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

import           Control.Applicative        ((*>), (<$>), (<*))
import           Control.Monad              (void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ReaderT, asks)
import qualified Data.ByteString.Char8      as BSC (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (hPutStr, pack)
import qualified Data.Text                  as T (Text, pack)
import           GHC.IO.Exception           (ExitCode (ExitFailure, ExitSuccess))
import           Network.Mail.Mime          (Address (Address),
                                             Encoding (QuotedPrintableText),
                                             Mail (mailHeaders, mailParts),
                                             Part (Part, partContent, partEncoding, partFilename, partHeaders, partType),
                                             emptyMail, renderMail')
import           System.Directory           (getTemporaryDirectory, removeFile)
import           System.IO                  (hClose)
import           System.IO.Temp             (openBinaryTempFile)
import           System.Process             (readProcessWithExitCode)
import           Test.HUnit.Base            (assertEqual, assertFailure)
import           Text.Parsec                (parse)
import           Text.Parsec.Char           (char, noneOf, string)
import           Text.Parsec.Combinator     (choice, many1)
import           Text.Parsec.Error          ()
import           Text.Parsec.Prim           (try)
import           Text.Parsec.String         (Parser)

data Config = Config {
    extensions :: String,
    sieveFile  :: FilePath
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

runSieveTestWithMail :: Mail -> ReaderT Config IO String
runSieveTestWithMail mail = do
  cfg_sieveFile <- asks sieveFile
  cfg_extensions <- asks extensions
  liftIO $ run cfg_sieveFile cfg_extensions
  where
    run :: FilePath -> String -> IO String
    run cfg_sieveFile cfg_extensions = do
      mailFile <- writeMailTemp mail
      result@(exitCode, stdout, _) <- readProcessWithExitCode "sieve-test" ["-x", cfg_extensions, cfg_sieveFile, mailFile] ""
      when (exitCode /= ExitSuccess) $ assertFailure $ formatFailure result mailFile
      removeFile mailFile
      return stdout
    formatFailure :: (ExitCode, String, String) -> FilePath -> String
    formatFailure (ExitFailure code, stdout, stderr) mailFile =
       "error code " ++ show code ++ " for sieve-test on '"
       ++ mailFile ++ "':\n"
       ++ "stdout:\n" ++ stdout
       ++ "\nstderr:\n" ++ stderr
    formatFailure _ _ = error "this should never happen :-)"

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
    void $ string "\nPerformed actions:\n\n"
    performedActions <- actionLines
    void $ string "\nImplicit keep:\n\n"
    implicitKeep <- actionLines
    void $ char '\n'
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
      void $ try $ string " (none)"
      return []
    -- sieve_result_action_printf in src/lib-sieve/sieve-result.c
    someAction :: Parser [Action]
    someAction = do
      void $ try $ string "* "
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
      void $ try $ string "       + "
      (:[]) <$> choice [
          createMailboxAction
        ]
    createMailboxAction :: Parser Action
    createMailboxAction = string "create mailbox if it does not exist" *> return CreateMailboxIfNotExist

assertMailActions :: Mail -> Actions -> ReaderT Config IO ()
assertMailActions mail expectedActions = do
    sieveTestOut <- runSieveTestWithMail mail
    liftIO $ do
      actualActions <- parseSieveTestOut sieveTestOut
      assertEqual ("unexpected Actions: " ++ sieveTestOut) expectedActions actualActions
  where
    parseSieveTestOut :: String -> IO Actions
    parseSieveTestOut s = case parse parseSieveTestResult "" s of
      (Left err) -> do
        assertFailure $ "could not parse output from sieve-test:\n"
          ++ show err
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
