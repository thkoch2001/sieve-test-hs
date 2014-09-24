module Main where

import Network.Sieve.Test
import Test.HUnit (runTestTT)
import Test.HUnit.Base (Test(..))

mailingListHeadersToTestCase :: ([(String, String)], String) -> Test
mailingListHeadersToTestCase (headers, folder) =
  TestCase $ assertHeadersStoredIn headers $ "list." ++ folder

mailingListHeadersTests :: Test
mailingListHeadersTests = TestList $ map f cases
  where
    f :: (String, String, String) -> Test
    f (name, value, folder) = mailingListHeadersToTestCase ([(name, value)], folder)
    cases :: [(String, String, String)]
    cases = [
      ("list-id", "lula@example.com", "lula"),
      ("x-list-id", "<hallo-welt\\.some-mailing.list.server", "hallo-welt"),
      ("mailing-list", "hi-there@local", "hi-there"),
      ("x-mailing-list", "x@x.x", "x"),
      ("x-listname", "<o@", "o"),

      -- mailman
      ("List-Id", "The Haskell-Beginners Mailing List - Discussion of primarily beginner-level topics related to Haskell <beginners.haskell.org>", "beginners"),
      -- Google groups
      ("List-ID", "<angular-ui.googlegroups.com>", "angular-ui"),
      -- Debian Bug Tracker
      ("List-Id", "<695666.bugs.debian.org>", "695666")
      ]

multipleMailingListHeadersTests :: Test
multipleMailingListHeadersTests = TestList $ map mailingListHeadersToTestCase cases
  where
    cases :: [([(String, String)], String)]
    cases = [
      -- Google groups
      ([("Mailing-list", "list angular-ui@googlegroups.com; contact angular-ui+owners@googlegroups.com"),
        ("List-ID", "<angular-ui.googlegroups.com>")
       ],
       "angular-ui"),
      -- Debian Bug Tracker
      ([("List-Id", "<695666.bugs.debian.org>"),
        ("X-Loop", "owner@bugs.debian.org")
       ],
       "695666"
      ),
      -- Debian Mailing Lists (@lists.debian.org), QMQP
      ([("X-Mailing-List", "<debian-haskell@lists.debian.org> archive/latest/5631"),
        ("X-Loop", "debian-haskell@lists.debian.org"),
        ("List-Id", "<debian-haskell.lists.debian.org>")
       ],
       "debian-haskell"
      ),
      -- Apache List, ezmlm
      ([("Mailing-List", "contact dev-help@community.apache.org; run by ezmlm"),
        ("List-Id", "<dev.community.apache.org>")
       ],
       "dev"
      ),
      -- Magnolia Dev List (includes JIRA spam)
      ([("List", "<dev-list@magnolia-cms.com>"),
        ("List-Id", "<dev-list.magnolia-cms.com>")
       ],
       "dev-list"),
      -- Debian package tracker subscription
      ([("X-Debian", "PTS"),
        ("X-Debian-Package", "docker.io"),
        ("List-ID", "<docker.io.bts.packages.qa.debian.org>")
       ],
       "docker"
      ),
      -- git
      ([("X-Mailing-List", "git@vger.kernel.org"),
        ("List-ID", "<git.vger.kernel.org>")
       ],
       "git"),
      -- linux@lug-erding.de
      ([("List-Id", "Mailingliste der LUG-Erding <linux.lug-erding.de>"),
        ("X-Mailing-List", "<linux@lug-erding.de> archive/latest/21995")
       ],
       "linux"),
      -- Yahoo Groups
      ([("Mailing-List", "list Moldawien@yahoogroups.de; contact Moldawien-owner@yahoogroups.de"),
        ("List-Id", "<Moldawien.yahoogroups.de>")
       ],
       "moldawien"),
      -- github notifications
      ([("List-ID", "angular-ui/ng-grid <ng-grid.angular-ui.github.com>"),
        ("X-GitHub-Recipient", "thkoch2001")
       ],
       "ng-grid"),
      -- mail chimp
      ([("X-Mailer", "MailChimp Mailer - **CIDa84ea70307ab6d1868f7**"),
        ("List-ID", "b4a4054cce715a3b0ae5e7d35mc list <b4a4054cce715a3b0ae5e7d35.87653.list-id.mcsv.net>")
       ],
       "b4a4054cce715a3b0ae5e7d35"
      )
      ]

allTests :: Test
allTests = TestList [
  mailingListHeadersTests,
  multipleMailingListHeadersTests
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
