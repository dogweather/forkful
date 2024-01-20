---
title:                "Skriver tester"
html_title:           "Haskell: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å skrive tester er en viktig del av utviklingsprosessen for programmerere. Dette involverer å lage små kodebiter som sjekker om visse deler av koden fungerer som de skal. Tester bidrar til å sikre at koden fungerer riktig og gjør det lettere å finne og rette feil.

## Hvordan:
For å skrive tester i Haskell, kan du bruke et rammeverk som Hspec eller QuickCheck. La oss se på et eksempel med Hspec:

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "adds two numbers" $ do
      add 2 4 `shouldBe` 6
```

Dette eksempelet sjekker om funksjonen "add" legger sammen to tall på riktig måte. Output vil være:

```Haskell
add
  - adds two numbers
```

Her får vi bekreftet at testen ble kjørt og at resultatet stemmer overens med forventningene våre.

## Dypdykk:
Å skrive tester har eksistert siden begynnelsen av programmering som en måte å sikre kvaliteten på kode. I Haskell er det vanlig å bruke rammeverk som Hspec eller QuickCheck for å skrive tester. Det finnes også alternative metoder som Property-based Testing som bruker matematisk logikk for å generere og sjekke tester.

Når det gjelder implementasjonen av tester, bruker Haskell funksjonell programmering og type-systemet sitt for å gjøre det enklere å skrive og vedlikeholde tester. Dette bidrar til å øke robustheten og kvaliteten på koden.

## Se også:
- [Hspec](https://hspec.github.io/)
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)