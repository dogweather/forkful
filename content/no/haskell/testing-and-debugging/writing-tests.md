---
title:                "Skrive tester"
date:                  2024-02-03T19:31:29.537183-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive tester i Haskell handler om å sikre at funksjonene dine fungerer som forventet gjennom automatiserte sjekker. Programmerere gjør det for å fange opp feil tidlig, lette refaktorering og dokumentere oppførsel, noe som gjør kodebasen mer vedlikeholdbar og skalerbar.

## Hvordan:

Haskell støtter forskjellige testrammeverk, men to populære er `Hspec` og `QuickCheck`. Hspec lar deg definere menneskelesbare spesifikasjoner for koden din, mens QuickCheck lar deg generere tester automatisk ved å beskrive egenskaper som koden din bør tilfredsstille.

### Bruke Hspec

Først, legg til `hspec` i konfigurasjonen til byggeverktøyet ditt (f.eks. `stack.yaml` eller `cabal`-filen). Deretter importerer du `Test.Hspec` og skriver tester som spesifikasjoner:

```haskell
-- fil: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "legger til to tall" $
    add 1 2 `shouldBe` 3

  it "returnerer det første tallet når man legger til null" $
    add 5 0 `shouldBe` 5
```

Deretter kjører du testene dine ved hjelp av byggeverktøyet ditt, noe som resulterer i en utskrift som kan se slik ut:

```
MyLib.add
  - legger til to tall
  - returnerer det første tallet når man legger til null

Fullført på 0.0001 sekunder
2 eksempler, 0 feil
```

### Bruke QuickCheck

Med QuickCheck uttrykker du egenskaper som funksjonene dine bør tilfredsstille. Legg til `QuickCheck` i prosjektkonfigurasjonen din, deretter importerer du den:

```haskell
-- fil: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Å kjøre disse testene vil automatisk generere inndata for å sjekke de angitte egenskapene:

```
+++ OK, besto 100 tester.
+++ OK, besto 100 tester.
```

I begge eksemplene med Hspec og QuickCheck fungerer testsuitene som kjørbar dokumentasjon som automatisk kan verifisere korrektheten av koden din.
