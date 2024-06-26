---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:53.156310-07:00
description: "Hur g\xF6r man: Haskell st\xF6der olika testramverk, men tv\xE5 popul\xE4\
  ra \xE4r `Hspec` och `QuickCheck`. Hspec l\xE5ter dig definiera m\xE4nniskol\xE4\
  sbara specifikationer f\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.958534-06:00'
model: gpt-4-0125-preview
summary: "Haskell st\xF6der olika testramverk, men tv\xE5 popul\xE4ra \xE4r `Hspec`\
  \ och `QuickCheck`."
title: Skriva tester
weight: 36
---

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
