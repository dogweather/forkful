---
title:                "Tests schreiben"
html_title:           "Kotlin: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Erstellung von Tests ist ein wesentlicher Bestandteil der Softwareentwicklung. Programmierer nutzen Tests, um sicherzustellen, dass ihr Code richtig funktioniert und um mögliche Fehler aufzudecken. Dadurch wird die Qualität der Software verbessert und die Wahrscheinlichkeit von Bugs und Problemen verringert.

## Wie geht's:
Um Tests in Kotlin zu schreiben, können wir die integrierte Test-Unterstützung von Kotlin verwenden. Wir erstellen eine Testklasse und fügen unsere Testmethoden hinzu. Anschließend können wir unsere Logik testen und durch definieren von erwarteten Ausgaben sicherstellen, dass unser Code wie gewünscht funktioniert.

```Kotlin
class MyTest {
    @Test fun testAddition() {
        assertEquals(4, 2 + 2)
    }
}
```

Dieses Beispiel testet eine einfache Addition und vergleicht das Ergebnis mit dem erwarteten Wert von 4. Wenn die Testmethode fehlschlägt, wird dies als Fehler im Testbericht angezeigt.

## Tiefgehender Einblick:
Tests spielen eine wichtige Rolle in der Geschichte der Softwareentwicklung und sind ein integraler Bestandteil von agilen Methoden wie Test-driven Development (TDD). Alternativen zu Test-Unterstützung in Kotlin sind beispielsweise JUnit und MockK.

Die Implementierung von Tests in Kotlin erfolgt durch Annotationen wie "@Test" und Assertions wie "assertEquals". Diese helfen dabei, Tests übersichtlich und leicht verständlich zu gestalten.

## Siehe auch:
- [JUnit](https://junit.org/junit5/)
- [MockK](https://mockk.io/)