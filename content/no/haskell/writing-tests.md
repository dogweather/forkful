---
title:                "Haskell: Å skrive tester"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Det å skrive tester er en viktig del av enhver programmeringsjobb. Det hjelper med å sikre at koden fungerer som den skal, og reduserer risikoen for feil i produksjon. Dette kan spare både tid og penger i det lange løp.

## How To

For å skrive tester i Haskell, kan du bruke et testbibliotek som HUnit eller QuickCheck. La oss se på et eksempel på hvordan man kan skrive en enkel enhetstest med HUnit:

```Haskell
module Main where

import Test.HUnit

-- Tester at summen av to tall er riktig
test_sum :: Test
test_sum = TestCase $ assertEqual "Summen av 2 og 3 skal være 5" 5 (2+3)

main :: IO Counts
main = runTestTT $ TestList [test_sum]
```

I dette eksempelet importerer vi testbiblioteket Test.HUnit, definerer en testfunksjon som sjekker om summen av 2 og 3 er lik 5, og kjører testen ved hjelp av funksjonen runTestTT.

Haskell har også et annet testbibliotek kalt QuickCheck, som bruker egenskaper i stedet for konkrete tester. La oss se på et eksempel på hvordan man kan bruke QuickCheck:

```Haskell
module Main where

import Test.QuickCheck

-- Tester om funksjonen f forandrer ikke på lengden av en liste
test_length :: [Int] -> Property
test_length list = (length list ==) . length $ map f list
  where f x = x + 1

main :: IO ()
main = quickCheck test_length
```

I dette eksempelet definerer vi en egenskapstest som sjekker om funksjonen f forandrer lengden på en liste. Vi bruker funksjonen quickCheck til å kjøre testen med tilfeldig genererte lister.

## Deep Dive

Å skrive gode tester er en viktig del av å lage robust og pålitelig programvare. Det er viktig å tenke gjennom hvilke deler av koden som trenger å bli testet, og hvordan man kan dekke alle mulige utfall.

Det er også viktig å gjøre testene enkle å vedlikeholde og utvide. Dette kan oppnås ved å følge god praksis som å skrive konsistente og beskrivende testnavn, og unngå avhengigheter mellom tester.

En annen viktig del av å skrive tester er å inkludere dem som en del av utviklingsprosessen. Ved å skrive tester mens man koder, kan man finne og fikse feil raskere, og sikre at koden ikke brytes når man gjør endringer.

## Se også

* [HUnit dokumentasjon](http://hackage.haskell.org/package/HUnit)
* [QuickCheck dokumentasjon](http://hackage.haskell.org/package/QuickCheck)
* [Haskell Wiki: Testing](https://wiki.haskell.org/Testing)