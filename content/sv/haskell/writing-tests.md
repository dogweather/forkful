---
title:                "Skriva tester"
aliases:
- sv/haskell/writing-tests.md
date:                  2024-02-03T19:30:53.156310-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Haskell handlar om att säkerställa att dina funktioner fungerar som förväntat genom automatiserade kontroller. Programmerare gör det för att upptäcka buggar tidigt, underlätta refaktorering, och dokumentera beteende, vilket gör kodbasen mer underhållbar och skalbar.

## Hur gör man:

Haskell stöder olika testramverk, men två populära är `Hspec` och `QuickCheck`. Hspec låter dig definiera människoläsbara specifikationer för din kod, medan QuickCheck låter dig generera tester automatiskt genom att beskriva egenskaper som din kod bör uppfylla.

### Använda Hspec

Först, lägg till `hspec` i din byggverktygskonfiguration (t.ex. `stack.yaml` eller `cabal`-fil). Importera sedan `Test.Hspec` och skriv tester som specifikationer:

```haskell
-- fil: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "lägger till två nummer" $
    add 1 2 `shouldBe` 3

  it "returnerar det första numret när noll läggs till" $
    add 5 0 `shouldBe` 5
```

Kör sedan dina tester med ditt byggverktyg, vilket resulterar i en output som kan se ut så här:

```
MyLib.add
  - lägger till två nummer
  - returnerar det första numret när noll läggs till

Avslutad på 0.0001 sekunder
2 exempel, 0 fel
```

### Använda QuickCheck

Med QuickCheck uttrycker du egenskaper som dina funktioner bör uppfylla. Lägg till `QuickCheck` i din projektinställning, importera den sedan:

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

När du kör dessa tester kommer ingångarna att genereras automatiskt för att kontrollera de specificerade egenskaperna:

```
+++ OK, godkända 100 tester.
+++ OK, godkända 100 tester.
```

I både Hspec- och QuickCheck-exemplen tjänar testsuiterna som exekverbar dokumentation som automatiskt kan verifiera korrektheten i din kod.
