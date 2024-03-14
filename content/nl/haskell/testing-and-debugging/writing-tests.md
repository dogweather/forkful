---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.596911-07:00
description: "Tests schrijven controleert of code doet wat het moet doen. Testen vangt\
  \ bugs op, helpt bij het onderhouden van code en zorgt ervoor dat wijzigingen niets\u2026"
lastmod: '2024-03-13T22:44:50.855929-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven controleert of code doet wat het moet doen. Testen vangt\
  \ bugs op, helpt bij het onderhouden van code en zorgt ervoor dat wijzigingen niets\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven controleert of code doet wat het moet doen. Testen vangt bugs op, helpt bij het onderhouden van code en zorgt ervoor dat wijzigingen niets breken.

## Hoe te:

Haskell gebruikt HUnit voor basis unit tests, en QuickCheck voor property-based tests. Hier is een snel voorbeeld van HUnit:

```haskell
import Test.HUnit

testList :: Test
testList = TestList [TestCase (assertEqual "Moet getallen optellen" 4 (2 + 2)),
                     TestCase (assertEqual "Moet getallen aftrekken" 0 (2 - 2))]

main :: IO Counts
main = runTestTT testList
```

Voer het uit, en het toont:

```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
```

Voorbeeld van QuickCheck:

```haskell
import Test.QuickCheck

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck prop_RevRev
```

Voorbeelduitvoer zou kunnen lezen:

```
+++ OK, 100 tests geslaagd.
```

## Diepere Duik

Testen begon met vroege programmering, maar werd serieus met de opkomst van TDD in de jaren '00. Haskell's pure functies maken het geweldig voor testen. Alternatieven voor HUnit/QuickCheck zijn doctest en Hedgehog. HUnit is vergelijkbaar met JUnit van Java. QuickCheck automatiseert het genereren van testcases, en controleert op eigenschappen die u definieert.

## Zie Ook

- HUnit documentatie: [http://hackage.haskell.org/package/HUnit](http://hackage.haskell.org/package/HUnit)
- QuickCheck op Hackage: [http://hackage.haskell.org/package/QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- Introductie tot Haskell Testen: [https://hspec.github.io/](https://hspec.github.io/)
- "Real World Haskell" door Bryan O'Sullivan, Don Stewart, en John Goerzen: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
