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

# mailchimp support 2017-04-13: [MailChimp] Re: MailChimp Support: FR: add header to mailchimp mails containing mailing list name (2094525)

Hey there Thomas,

Thanks so much for reaching out to us here in Support! I'll be more than happy to answer your question.

To provide some clarity, we wouldn't be able to modify that header information to read as "webopsweekly" when sending campaigns. With that said, something I can recommend would be to use the envelope-from information instead. This information would be formatted like:

    bounce-mc.{dc}_{userID}.{campaign_id}-{local_part_of_rcpt's_email_address}={rcpt's_domain}@{sending_mta}

The unchanging element in that string would be the userID, and that would be what you would want to set your filtering based on to get campaigns to automatically route to your designated folder.

Please feel more than free to let me know if there's anything else I can help with.

Have an awesome rest of your day!

Thank You,
Savannah