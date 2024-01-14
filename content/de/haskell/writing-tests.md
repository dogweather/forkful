---
title:                "Haskell: Test schreiben"
simple_title:         "Test schreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es eine Vielzahl von Meinungen darüber, ob Tests bei der Entwicklung von Software notwendig sind oder nicht. Einige argumentieren, dass Tests zeitaufwändig und unnötig sind, während andere sagen, dass sie unerlässlich sind für die Sicherstellung der Qualität des Codes. Aber warum sollte man überhaupt Tests schreiben?

Der Hauptgrund dafür ist, dass Tests dazu beitragen, Fehler im Code frühzeitig zu erkennen. Sie ermöglichen es, potenzielle Probleme zu identifizieren und zu beheben, bevor sie zu größeren Problemen werden, die möglicherweise erst in der Produktionsumgebung auftreten. Sie dienen als eine Art Sicherheitsnetz für Ihren Code und geben Ihnen die Gewissheit, dass alles wie erwartet funktioniert.

## Wie man Tests schreibt
Um Tests in Haskell zu schreiben, müssen wir uns zunächst mit dem Konzept des "QuickCheck" vertraut machen. QuickCheck ist eine Haskell Library, die es ermöglicht, automatisierte Tests zu schreiben, die zufällige Eingaben generieren und überprüfen, ob die Ausgabe wie erwartet ist.

Um QuickCheck zu verwenden, müssen wir zuerst die `Test.QuickCheck` Module importieren und dann unsere Testfunktion definieren. Wir können dies mit dem folgenden Code tun:

```Haskell
import Test.QuickCheck

-- Beispieltestfunktion
additionTest :: Int -> Int -> Bool
additionTest x y = x + y == y + x

-- Test ausführen
quickCheck additionTest
```

Wenn wir diesen Code ausführen, erhalten wir die Ausgabe `+++ OK, passed 100 tests.` Dies bedeutet, dass unsere Funktion in 100 zufälligen Fällen erfolgreich war.

Wir können auch benutzerdefinierte Datentypen verwenden, um spezifischere Tests zu schreiben. Zum Beispiel können wir eine benutzerdefinierte Datentyp `Person` definieren und dann einen Test schreiben, der überprüft, ob das Alter einer Person in einem bestimmten Bereich liegt.

```Haskell
data Person = Person String Int

ageRangeTest :: Person -> Bool
ageRangeTest (Person name age) = age >= 18 && age <= 65

-- Test ausführen
quickCheck ageRangeTest
```

Die Ausgabe würde in diesem Fall `+++ OK, passed 100 tests.` oder `*** Failed! Falsifiable (after 4 tests):` sein, je nachdem, ob die Funktion in allen oder nur einigen zufälligen Fällen erfolgreich war.

## Tiefere Einblicke
Wenn es darum geht, Tests zu schreiben, gibt es viele verschiedene Ansätze und Methoden, die man wählen kann. Einige Entwickler bevorzugen Unit-Tests, bei denen jede Funktion oder jede Klasse einzeln getestet wird. Andere bevorzugen Integrationstests, bei denen das Zusammenspiel mehrerer Funktionen getestet wird.

Es gibt auch die Möglichkeit, Eigenschaftstests zu schreiben, bei denen die Funktion eines Programms auf Eigenschaften getestet wird, anstatt konkrete Eingaben zu überprüfen. Dies kann besonders nützlich sein, wenn es darum geht, Komplexität und unerwartete Verhaltensweisen zu erkennen.

Egal, für welchen Ansatz man sich entscheidet, die Hauptsache ist, dass Tests dazu beitragen, zu garantieren, dass unser Code funktioniert und robust ist.

## Siehe auch
- [Haskell QuickCheck-Dokumentation](https://hackage.haskell.org/package/QuickCheck)
- [Writing Testable Haskell Code](https://www.parsonsmatt.org/2018/10/08/writing_testable_code_in_haskell.html)
- [Introduction to Property Tests in Haskell](https://jadpole.github.io/introduction-to-property-testing-in-haskell/)