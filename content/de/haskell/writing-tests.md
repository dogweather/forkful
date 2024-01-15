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

## Warum

Warum jemand Tests schreiben sollte? Ganz einfach: um sicherzustellen, dass der Code, den wir schreiben, fehlerfrei funktioniert und den Anforderungen entspricht. Tests sind ein unverzichtbarer Bestandteil von sauberem und zuverlässigem Code.

## Wie geht's

Coding-Beispiele und Ausgabebeispiele werden in "```Haskell ...```" Codeblöcken gezeigt.

Das Schreiben von Tests in Haskell ist relativ einfach. Wir können die `HUnit` Bibliothek verwenden, um unsere Tests zu schreiben. Hier ist ein Beispiel für einen einfachen Additions-Test:

```Haskell
import Test.HUnit

-- Unsere Testfunktion
testAddition = TestCase (assertEqual "1 + 1 sollte 2 ergeben" (1+1) 2)

-- Die eigentliche Testsuite
tests = TestList [testAddition]

-- Ausführung der Tests
runTests = runTestTT tests
```

Die Ausgabe sollte folgendermaßen aussehen:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: x  Tried: 1  Errors: 0  Failures: 0
```

Wir können auch spezifische Fehlerausgaben für unsere Tests definieren, um die Probleme leichter zu erkennen:

```Haskell
testDivision = TestCase (assertEqual "5 / 0 sollte einen Fehler ausgeben" (5/0) (error "Division durch Null"))

tests = TestList [testDivision]
```

Die Ausgabe für diesen Test wäre:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 1
Cases: x  Tried: 1  Errors: 0  Failures: 1
### Failure in: 0: testDivision
does not match expected value of:
<exception thrown>
+ expected:
<exception thrown>
+ but got:
5.0e10^100
```

Es ist auch möglich, mehrere Tests zu gruppieren, um sie einfacher zu verwalten und auszuführen. Hier ist ein Beispiel für eine Testsuite mit mehreren Tests:

```Haskell
import Test.HUnit

testAddition = TestCase (assertEqual "1 + 1 should return 2" (1+1) 2)
testMultiplication = TestCase (assertEqual "2 x 3 should return 6" (2*3) 6)
testSubtraction = TestCase (assertEqual "5 - 7 should return -2" (5-7) (-2))

tests = TestList [testAddition, testMultiplication, testSubtraction]

runTests = runTestTT tests
```

Und hier ist die entsprechende Ausgabe:

```
Cases: 3  Tried: 3  Errors: 0  Failures: 0
Cases: x  Tried: 3  Errors: 0  Failures: 0
```

Es gibt noch viele weitere Möglichkeiten, Tests in Haskell zu schreiben und auszuführen. Wir können auch `QuickCheck` verwenden, um automatische Eigenschaftstests zu erstellen oder `Test.Tasty` für eine noch komplexere Teststruktur. Weitere Ressourcen finden Sie in der "Siehe auch" Sektion.

## Tiefergehende Details

Das Schreiben von Tests ist ein wichtiger Bestandteil von gutem Code und sollte von Anfang an in den Entwicklungsprozess einbezogen werden. Es ermöglicht uns, die Funktionalität unseres Codes zu überprüfen und sicherzustellen, dass er korrekt funktioniert und robust ist. Mit den richtigen Tools und ein wenig Übung können wir saubere und zuverlässige Tests erstellen, die uns helfen, besseren Code zu schreiben.

## Siehe auch

- [HUnit Dokumentation](https://hackage.haskell.org/package/HUnit)
- [QuickCheck Dokumentation](https://hackage.haskell.org/package/QuickCheck)
- [Test.Tasty Dokumentation](https://hackage.haskell.org/package/tasty)