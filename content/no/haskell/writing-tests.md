---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester er å lage kode for å sjekke at annen kode virker som den skal. Programmerere gjør dette for å finne feil, forhindre fremtidige feil, og sikre at programmet oppfører seg riktig under forskjellige forhold.

## How to:
Haskell bruker `Hspec` som et populært testrammeverk. Installere det med `cabal install hspec`. Her er et enkelt eksempel:

```Haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "length" $ do
    it "returns the number of elements in a list" $ do
      length [1, 2, 3] `shouldBe` 3

  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1, 2, 3] `shouldBe` [3, 2, 1]

    it "gives an empty list when reversing an empty list" $ do
      reverse [] `shouldBe` ([] :: [Int])
```

Kjør testene med `runhaskell test.hs`. Output burde se slik ut:

```
length
  returns the number of elements in a list
reverse
  reverses a list
  gives an empty list when reversing an empty list

Finished in 0.0004 seconds
3 examples, 0 failures
```

## Dykk Ned:
Testing i Haskell startet med `QuickCheck`, en pioner for property-baserte tester. `Hspec` er mer BDD-orientert (Behavior-Driven Development). Alternativer inkluderer `HUnit` for unit-testing. Når du skriver tester, tenk på grenseverdier, typen null-tilfeller, og hvordan koden skal håndtere uventet input.

## Se Også:
- Hspec dokumentasjon: [http://hspec.github.io/](http://hspec.github.io/)
- Learn You a Haskell for Great Good! (testing chapter): [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- Stackage Server, for å finne biblioteker: [https://www.stackage.org/](https://www.stackage.org/)
