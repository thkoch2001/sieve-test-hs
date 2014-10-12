Test suite for my SIEVE mail filters

This is my first project in Haskell. I'd appreciate any feedback on how to
clean up the code.

The main.hs file contains tests for my personal mail filter. But the
Network.Sieve.Test module should be reusable for anybody. I'd like to publish
it on hackage after some more cleanup.

# TODO

* learn how to write haskell inline documentation
* separate the tests from the test runner
* make this a library for hackage
* parse all possible output (actions) of sieve-test
  * included:
    * store in folder
    * create folder if not exists
    * implicit keep
    * discard
  * missing
    * ...?

