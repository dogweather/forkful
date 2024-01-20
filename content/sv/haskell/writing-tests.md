---
title:                "Skriva tester"
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester handlar om att bekräfta att kod gör vad den ska. Programmerare gör det för att snabbt hitta buggar, förbättra design och säkerställa kodkvaliteten över tid.

## Hur gör man?:
I Haskell kan vi använda `Hspec`, ett testbibliotek som underlättar beteendedriven utveckling (BDD). Här är ett enkelt exempel:

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "correctly adds two numbers" $ do
      (1 + 1) `shouldBe` 2
```

Kör testet och få detta resultat:
```
addition
  correctly adds two numbers
Finished in 0.0001 seconds
1 example, 0 failures
```

## Djupdykning
Testning i Haskell startade tidigt med `QuickCheck` som skapades runt 2000. Alternativ till `Hspec` inkluderar `QuickCheck` för egenskapstester och `Tasty` för en modulär testsvit. `Hspec` bygger på `HUnit` och lägger till en BDD-liknande syntax. `Hspec` kan kombineras med `QuickCheck` för att få fördelarna med båda.

## Se också
- Hspec dokumentation: http://hspec.github.io/
- QuickCheck på Hackage: https://hackage.haskell.org/package/QuickCheck
- Tasty testramverk: https://hackage.haskell.org/package/tasty