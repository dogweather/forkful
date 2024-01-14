---
title:                "Haskell: Tests schreiben"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind eine wichtige und notwendige Komponente in der Softwareentwicklung. Sie ermöglichen es Programmierern, die Funktionalität ihrer Codes zu überprüfen und sicherzustellen, dass sie korrekt funktionieren. Das Schreiben von Tests ist ein wichtiger Schritt, um sicherzustellen, dass die Software fehlerfrei ist und den Erwartungen der Benutzer entspricht.

## Wie man Tests schreibt

Das Schreiben von Tests in Haskell ist ein relativ einfacher Prozess. Zunächst müssen Sie das Test-Framework `HUnit` importieren, indem Sie folgende Zeile zu Ihrem Code hinzufügen:

```Haskell
import Test.HUnit
```

Als nächstes definieren Sie eine Testfunktion, die die Funktionalität testen soll, zum Beispiel eine Funktion `double`, die eine Zahl verdoppelt:

```Haskell
double :: Int -> Int
double x = x * 2
```

Dann können Sie eine Liste von Tests erstellen, die überprüfen, ob die Funktion korrekt funktioniert:

```Haskell
doubleTests :: [Test]
doubleTests = 
    [ 
        "Double of 5 should be 10" ~: double 5 ~?= 10, 
        "Double of 0 should be 0" ~: double 0 ~?= 0 
    ] 
```

Die Liste enthält Testfälle, die jeweils einen Textbeschreibung des Tests, das erwartete Ergebnis und das tatsächliche Ergebnis enthalten. Schließlich können Sie die Tests mit der `runTestTT` Funktion ausführen:

```Haskell
main :: IO () 
main = do 
    runTestTT $ TestList doubleTests 
```

Dies wird die Ausgabe der Tests in der Konsole zeigen, die besagt, ob die Tests erfolgreich sind oder nicht.

## Deep Dive

Tests in Haskell werden normalerweise in einer eigenen Modul-Datei geschrieben, die als `Spec.hs` bezeichnet wird. Diese Datei sollte die gleichen Import-Anweisungen wie die Hauptdatei Ihres Projekts enthalten, sowie alle notwendigen Testfunktionen und Testlisten.

Das `HUnit` Framework bietet auch die Möglichkeit, modulare Tests zu schreiben, die in Untergruppen organisiert sind, um die Lesbarkeit und Verwaltbarkeit zu verbessern. Sie können auch Eigenschaftsbasierte Tests schreiben, die zufällige Eingabewerte verwenden, um die Funktionalität Ihrer Codes weiter zu überprüfen.

Zusätzlich zu `HUnit` gibt es auch andere Test-Frameworks wie `QuickCheck`, die in Haskell verwendet werden können. Es lohnt sich, verschiedene Frameworks auszuprobieren und das zu finden, das am besten zu Ihrem Projekt passt.

## Siehe auch

- [HUnit Dokumentation](https://hackage.haskell.org/package/HUnit)
- [QuickCheck Dokumentation](https://hackage.haskell.org/package/QuickCheck)
- [Einführung in das Unit-Testing in Haskell](https://www.stackage.org/lts-14.19/package/hunit-1.6.0.0/docs/Test-HUnit.html)