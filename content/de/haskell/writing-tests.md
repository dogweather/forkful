---
title:    "Haskell: Test schreiben"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil im Programmierprozess, da sie sicherstellen, dass der Code funktioniert wie erwartet. Durch das Schreiben von Tests können Fehler frühzeitig erkannt und behoben werden, was letztendlich zu einem stabilen und zuverlässigen Programm führt.

## Wie man Tests in Haskell schreibt

Das Schreiben von Tests in Haskell ist einfach und effektiv. Zunächst müssen wir das Paket "HUnit" importieren, welches uns die nötigen Funktionen zur Erstellung von Tests zur Verfügung stellt. Wir definieren eine Funktion, die wir testen möchten:

```Haskell
add x y = x + y
```

Anschließend können wir unsere Testfälle definieren und ausführen:

```Haskell
import Test.HUnit

testAdd = TestCase (assertEqual "Addition von 2 & 3" 5 (add 2 3))
tests = TestList [testAdd]

runTests = putStrLn "\nTests:" >> runTestTT tests

```

Der Output sollte folgendermaßen aussehen:

```
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

Dies bedeutet, dass unser Test erfolgreich war und die Funktion wie erwartet funktioniert.

## Tiefergehende Informationen zum Testen in Haskell

Neben dem Paket "HUnit" gibt es auch noch andere Möglichkeiten, um Tests in Haskell zu schreiben, z.B. mit "QuickCheck" oder "Tasty". Es gibt auch die Möglichkeit, eigene Test-Frameworks zu erstellen. Wichtig ist vor allem, dass die Tests klar und verständlich strukturiert sind und alle möglichen Fälle abgedeckt werden.

## Siehe auch

- [HUnit Dokumentation](https://hackage.haskell.org/package/HUnit)
- [QuickCheck Dokumentation](https://hackage.haskell.org/package/QuickCheck)
- [Tasty Dokumentation](https://hackage.haskell.org/package/tasty)