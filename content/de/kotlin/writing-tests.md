---
title:    "Kotlin: Tests schreiben"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein unverzichtbarer Bestandteil einer qualitativ hochwertigen Softwareentwicklung. Sie dienen dazu, Fehler frühzeitig zu erkennen, die Funktionalität einer Anwendung zu überprüfen und sicherzustellen, dass zukünftige Änderungen die bestehende Funktionalität nicht beeinträchtigen. Durch das Schreiben von Tests können Entwickler Zeit und Geld sparen, indem sie potenzielle Probleme im Voraus identifizieren und beheben.

## How To

Um in Kotlin Tests zu schreiben, müssen Sie zuerst das Testframework JUnit in Ihrem Projekt hinzufügen. Anschließend können Sie Tests erstellen, indem Sie eine Funktion mit dem Annotation "@Test" erstellen und die zu testende Funktionalität innerhalb dieser Funktion aufrufen. Hier ist ein Beispiel für eine einfache Funktion, die addieren kann:

```Kotlin
@Test
fun addTest() {
    val result = add(2, 3)
    assertTrue(result == 5)
}
```

Zuerst wird die Funktion "addTest" mit der Annotation "@Test" gekennzeichnet. Dann wird die add-Funktion aufgerufen und das Ergebnis mit der Funktion "assertTrue" überprüft, ob es der erwarteten Summe entspricht.

## Deep Dive

Beim Schreiben von Tests ist es wichtig, verschiedene Szenarien und Randfälle zu berücksichtigen, um sicherzustellen, dass die Anwendung in allen Fällen korrekt funktioniert. Auch ist es von Vorteil, Assertions zu verwenden, um sicherzustellen, dass die tatsächlichen Ergebnisse mit den erwarteten übereinstimmen. Zusätzlich können Mock-Objekte verwendet werden, um externe Abhängigkeiten zu simulieren und isolierte Tests durchzuführen.

Ein weiterer wichtiger Aspekt ist die kontinuierliche Integration (CI). Durch die Integration von Tests in den CI-Prozess können Entwickler sicherstellen, dass ihre Code-Änderungen nicht die bestehende Funktionalität beeinträchtigen und die Anwendung weiterhin zuverlässig bleibt.

## Siehe auch

- [JUnit documentation] (https://junit.org/junit5/docs/current/user-guide/)
- [Kotlin Test documentation] (https://kotlinlang.org/docs/tutorials/getting-started.html)
- [Test Driven Development (TDD) in Kotlin] (https://developer.android.com/training/testing/tdd)