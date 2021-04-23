# Caduceus

![Caduceus](/assets/caduceus.png)

In Greek and Roman mythology, [caduceus](https://en.wikipedia.org/wiki/Caduceus) was the wand used by Hermes (Mercury): the god of financial gain, commerce, eloquence, messages, communication, travelers, boundaries, luck, trickery and thieves.  Caduceus was given to Hermes by Apollo as a gesture of friendship between the two gods, and over time it has come to represent commerce and negotiation; two realms in which balanced exchange and reciprocity are recognized as ideals.  

In international banking, one of the ways that reciprocity across banks is encoded is through the [International Bank Account Number (IBAN)](https://en.wikipedia.org/wiki/International_Bank_Account_Number) system, which is the international standard for identifying bank accounts across borders.  Through IBAN, banks across the world have the power to send money to each other and ensure that the money is going to the correct place.  

However, as with many international standards, IBANs can be complex, and verifying the correctness of these IBANs during financial transactions is critical.  This is the intended use case of caduceus (this library): to help Haskellers validate and parse IBAN & SWIFT codes out-of-the-box so that they can write business logic without worrying about if the IBANs that they're dealing with are correct.  Just as Hermes used his caduceus to facilitate transactions in mythology, so too can you, a humble software engineer, use caduceus help to build resilient services for doing international banking.  Happy hacking!

## Usage

This library isn't on Hackage yet since it's still in alpha; I'll upload it to Hackage once it's ready for wider use.  If you want to use it now, though, you can build it from source.  You'll need to have [stack](https://docs.haskellstack.org/en/stable/README/) and [cabal](https://www.haskell.org/cabal/) installed locally, but if you do, you can just run `stack build` from the root of this project.  

At this point, caduceus exposes the `Wires.IBAN` module (which lets you parse IBANs via `parseIBAN`, pretty-print them via `prettyIBAN`, and verify the IBAN country via `country`); and the `Wires.SWIFT` modules (which lets you parse SWIFT codes via `parseSWIFT` and verify the SWIFT country via `country`.  Pretty-printing is still a work in progress).

## Future Work

Coming soon I want to implement a parser for MT103 receipts, which are [notoriously complicated to parse](https://www.immagic.com/eLibrary/ARCHIVES/GENERAL/IONA_IE/I070510D.pdf).  Here's hoping that adventure goes well!
