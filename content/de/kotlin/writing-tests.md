---
title:                "Kotlin: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es viele Meinungen darüber, ob es sinnvoll ist, Tests zu schreiben. Einige argumentieren, dass es Zeitverschwendung ist und dass man stattdessen mehr Funktionen implementieren sollte. Aber diejenigen, die sich für das Schreiben von Tests entscheiden, wissen die Vorteile zu schätzen.

Writing tests hilft, sicherzustellen, dass der Code, den wir schreiben, funktioniert und richtig arbeitet. Es gibt uns auch die Sicherheit, dass Änderungen, die wir in unserem Code vornehmen, keine neuen Fehler einführen. Durch das Schreiben von Tests können wir auch die Qualität unseres Codes verbessern und letztendlich Zeit und Geld sparen.

## Wie man Tests schreibt

Angenommen, wir möchten eine einfache Funktion in Kotlin schreiben, die prüft, ob eine Zahl gerade oder ungerade ist. Hier ist ein Beispiel, wie wir dies mit Tests umsetzen können:

```Kotlin
// Funktion, die überprüft, ob eine Zahl gerade ist
fun isEven(number: Int): Boolean {
    return number % 2 == 0
}
```

```Kotlin
// Import der Testbibliothek
import org.junit.Test
// Import der Assertionsbibliothek
import org.junit.Assert.*

// Der Testfall für die Funktion isEven
@Test
fun testIsEven() {
    // Arrange
    val number = 6
    
    // Act
    val result = isEven(number)
    
    // Assert
    assertTrue(result)
}
```

Das obige Beispiel zeigt, wie wir die Funktion testen können, um sicherzustellen, dass sie das erwartete Ergebnis liefert. Wir importieren die Test- und Assertionsbibliotheken, definieren einen Testfall und verwenden dann die `assertTrue` Assertion, um sicherzustellen, dass unser Ergebnis wahr ist.

## Tiefere Einblicke ins Schreiben von Tests

Es gibt viele verschiedene Arten von Tests, die wir schreiben können, einschließlich Unit-Tests, Integrationstests und Akzeptanztests. Jeder Typ hat seine eigenen Stärken und Zwecke, aber sie alle dienen dazu, die Funktionalität und Qualität unseres Codes zu überprüfen.

Ein weiterer wichtiger Aspekt beim Schreiben von Tests ist die Tatsache, dass es uns hilft, saubereren und wiederverwendbaren Code zu schreiben. Durch TDD (Test-driven development) können wir zuerst unsere Tests schreiben und dann unseren Code danach entwerfen, um sicherzustellen, dass er den Anforderungen entspricht. Dies kann dazu beitragen, Code-Duplikationen zu vermeiden und die Wartbarkeit des Codes zu verbessern.

Schließlich ermöglichen Tests auch eine bessere Zusammenarbeit innerhalb eines Teams. Indem wir Tests schreiben, können wir sicherstellen, dass alle Änderungen, die wir in unserem Code vornehmen, keine Auswirkungen auf andere Teile des Systems haben und somit eine reibungslose Integration ermöglichen.

## Siehe auch

- [Why Writing Tests is Important](https://dzone.com/articles/why-writing-tests-is-important)
- [Kotlin Testing - Unit Test with JUnit and Mockk](https://www.baeldung.com/kotlin-testing-junit-mockk)
- [Test-driven development (TDD)](https://www.agilealliance.org/glossary/tdd/)