---
title:    "Haskell: Tests schreiben"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Warum Tests schreiben?

Tests schreiben ist ein wichtiger Aspekt der Programmierung, der oft unterschätzt wird. Sie helfen dabei, die Qualität und Funktionalität des Codes zu verbessern, indem sie Fehler aufdecken und sicherstellen, dass Änderungen keine unerwünschten Nebenwirkungen haben.

# Wie man Tests in Haskell schreibt

Um Tests in Haskell zu schreiben, gibt es zwei wichtige Konzepte: das Testframework und die Assertion Library.

Das Testframework ist verantwortlich für das Ausführen der Tests und das Sammeln von Ergebnissen. Eines der beliebtesten Frameworks für Haskell ist Hspec, aber es gibt auch weitere Optionen wie QuickCheck oder doctest.

Die Assertion Library ermöglicht es uns, Bedingungen zu definieren, die in unseren Tests gelten sollten. Hier ein Beispiel, wie man mit Hspec eine einfache Test-Suite schreibt:

```Haskell
module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        it "adds two numbers correctly" $ do
            (2 + 2) `shouldBe` 4
```

Man kann sehen, dass die Test-Suite mit `describe` und `it` Blöcke strukturiert wird und die Bedingung mit der `shouldBe` Funktion definiert wird. Diese Bedingung erwartet, dass der Ausdruck links vom Gleichheitszeichen das gleiche Ergebnis hat wie der Ausdruck auf der rechten Seite.

Wenn wir nun diese Datei ausführen, sehen wir die Ausgabe:

```
addition
  adds two numbers correctly

Finished in 0.0019 seconds
1 example, 0 failures
```

# Tiefer Einblick in das Schreiben von Tests

Es gibt viele weitere Funktionen und Möglichkeiten, die das Testen in Haskell ermöglichen. Hier einige hilfreiche Ressourcen, um mehr über das Schreiben von Tests zu erfahren:

- [The Hspec User Guide](https://hspec.github.io)
- [QuickCheck Dokumentation](https://hackage.haskell.org/package/QuickCheck)
- [Doctest Tutorial](https://github.com/sol/doctest#tutorial)

Du kannst auch die Test-Suiten anderer Open-Source-Projekte durchsehen, um zu sehen, wie sie ihre Tests implementiert haben. Das wird dir helfen, ein Gefühl dafür zu bekommen, wie man effektive Tests schreibt.

# Siehe auch

- [The Importance of Testing in Software Development](https://www.freecodecamp.org/news/why-we-need-to-write-tests/) (auf Englisch)
- [Unit Tests for Haskell Projects](https://www.haskellforall.com/2020/12/unit-tests-for-haskell-projects.html) (auf Englisch)
- [Tdd-kata.de](https://tdd-kata.de) (auf Deutsch)