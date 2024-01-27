---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Programmieren sind Tests entscheidend, um sicherzustellen, dass Code korrekt funktioniert. Sie helfen dabei, Fehler frühzeitig zu erkennen und sorgen dafür, dass Änderungen nicht unbeabsichtigt andere Teile der Software stören.

## How to:
Installiere die `HUnit`-Testbibliothek mit `cabal install HUnit`. Schreibe dann einen einfachen Funktionstest:

```Haskell
import Test.HUnit

-- Eine einfache Funktion, die getestet wird
addiere :: Int -> Int -> Int
addiere x y = x + y

-- Ein Testfall für die Funktion
testAddiere1 :: Test
testAddiere1 = TestCase (assertEqual "Fuer (addiere 2 2)," 4 (addiere 2 2))

-- Hauptfunktion, die alle Tests ausführt
main :: IO Counts
main = runTestTT testAddiere1

```

Ausführen des Tests liefert:
```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

## Deep Dive
HUnit ist inspiriert vom JUnit-Framework und ermöglicht das Schreiben von Unit-Tests in Haskell. Alternativen sind QuickCheck für Property-basiertes Testen und Tasty als Testframework, das verschiedene Testansätze integriert. Effektives Testen erfordert Verständnis der zu testenden Funktionen und deren Edge-Cases. Historisch gesehen hat Test-Driven Development (TDD), welches das Schreiben von Tests vor dem eigentlichen Code beinhaltet, zur Entwicklung robuster Software-Systeme beigetragen.

## See Also
- [HUnit-Dokumentation](http://hackage.haskell.org/package/HUnit)
- [QuickCheck auf Hackage](http://hackage.haskell.org/package/QuickCheck)
- [Tasty auf Hackage](http://hackage.haskell.org/package/tasty)
- [Ein Haskell Testing Tutorial](https://wiki.haskell.org/Testing)
