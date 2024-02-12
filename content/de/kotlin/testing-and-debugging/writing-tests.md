---
title:                "Tests Schreiben"
date:                  2024-02-03T19:31:03.978546-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Tests in Kotlin beinhaltet das Erstellen von Code-Snippets, die automatisch die funktionale Korrektheit Ihrer Softwaremodule validieren und sicherstellen, dass sie wie erwartet funktionieren. Programmierer tun dies, um frühzeitig Fehler zu erkennen, die Code-Umstrukturierung zu erleichtern und eine Dokumentation darüber zu bieten, wie die Softwarekomponenten arbeiten sollen.

## Wie geht das:

Kotlin unterstützt die testgetriebene Entwicklung mit verschiedenen Frameworks, wobei JUnit, Kotest und MockK für das Mocking am beliebtesten sind. Hier ist ein einfaches Beispiel mit JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `fügt zwei Zahlen hinzu`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Beispielausgabe**

```text
Test bestanden.
```

Für einen ausgefeilteren Testansatz mit Kotest, das einen idiomatischeren Kotlin-Test-Schreibstil bietet, siehe das Beispiel unten:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "Das Hinzufügen von 2 und 3 sollte 5 ergeben" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Verwendung von MockK für Tests mit Mocks:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `getData gibt gemockte Daten zurück`() {
        every { repository.getData() } returns "Gemockte Daten"

        val result = service.getData()

        assertEquals("Gemockte Daten", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Beispielausgabe**

```text
Test bestanden.
```

Diese Beispiele veranschaulichen die Grundlagen des Schreibens von Unit-Tests in Kotlin. Wenn Ihre Anwendung wächst, sollten Sie erwägen, fortgeschrittenere Testtechniken und -tools zu erkunden, die von jedem Framework bereitgestellt werden.
