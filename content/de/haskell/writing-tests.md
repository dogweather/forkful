---
title:                "Tests schreiben"
html_title:           "Haskell: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
---

{{< edit_this_page >}}

Was sind Tests und warum sind sie wichtig?

Tests sind ein wichtiger Bestandteil in der Softwareentwicklung. Sie sind speziell geschriebene Code-Segmente, die die korrekte Funktionalität von anderen Code-Segmente, oder auch ganze Programme, überprüfen. Programmierer verwenden Tests, um sicherzustellen, dass ihr Code korrekt, robust und zuverlässig ist.

Wie führt man Tests in Haskell durch?

Tests in Haskell können mit dem Framework "Hspec" durchgeführt werden. Dabei werden für jede zu testende Funktion, Daten oder Code-Segment eine oder mehrere Testfälle erstellt, die überprüfen, ob die erwarteten Ergebnisse erzielt werden. Hier ist ein Beispielcode, der die Funktionsweise von "Hspec" demonstriert:

```Haskell
main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "adds two numbers correctly" $ do
      add 2 2 `shouldBe` 4

-- Beispiel-Funktion für die Addition von zwei Zahlen
add :: Int -> Int -> Int
add x y = x + y
```

Die Ausgabe sieht dann so aus:

```
addition
  adds two numbers correctly

Finished in 0.0010 seconds
1 example, 0 failures
```

Tiefere Einblicke in Tests in Haskell

Das Konzept von automatisierten Tests wird bereits seit den 1950er Jahren in der Softwareentwicklung verwendet, um die Qualität von Programmen zu verbessern. Es gibt auch andere Frameworks wie "QuickCheck", welches auf zufallsgenerierten Eingaben basiert, um die Funktionalität von Code zu testen.

In Haskell kann auch das Modul "Test.HUnit" verwendet werden, welches auf Unit-Tests spezialisiert ist. Es bietet ähnliche Funktionen wie "Hspec" aber mit einer etwas anderen Syntax.

Weiterführende Links

- Offizielle Dokumentation von "Hspec": https://hspec.github.io/
- "QuickCheck" Framework: https://hackage.haskell.org/package/QuickCheck
- "Test.HUnit" Modul: https://hackage.haskell.org/package/HUnit