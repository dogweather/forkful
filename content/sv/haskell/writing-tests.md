---
title:                "Haskell: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla Haskell-kod. Det ger dig möjlighet att hitta och åtgärda buggar tidigt i utvecklingsprocessen och ger en extra säkerhetsnivå för din kod.

## Hur man gör

För att skriva tester i Haskell behöver du använda ett testbibliotek, som till exempel HUnit eller QuickCheck. Låt oss titta på ett exempel på hur man kan skriva och köra tester med HUnit.

```Haskell
-- Importera HUnit biblioteket
import Test.HUnit

-- En funktion som vi vill testa
add :: Int -> Int -> Int
add x y = x + y

-- Testfall för funktionen add
testAdd :: Test
testAdd = TestCase $ assertEqual "Should be 10" (add 5 5) 10

-- En grupp av testfall
addTests :: Test
addTests = TestList [testAdd]

-- Köra testerna
main :: IO Counts
main = runTestTT addTests
```

Överstående koddefinierar en funktion "add" och ett testfall som kontrollerar om funktionen ger önskat resultat. Testet körs sedan i huvudfunktionen.

## Djupdykning

Haskell har ett starkt typsystem, vilket gör att kod som kompilerar oftast fungerar som förväntat. Men tester hjälper till att upptäcka fel som kanske inte syns vid en första anblick. Det är också bra att skriva tester för gränssnittet av din kod, eftersom dessa kan förändras i framtiden och det är viktigt att se till att allt fortfarande fungerar som det ska.

## Se även

- [HUnit](https://hackage.haskell.org/package/HUnit)
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
- [Haskell Testing Best Practices](https://wiki.haskell.org/Testing_guidelines)