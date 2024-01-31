---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben dient dazu, Code auf seine Korrektheit zu überprüfen. Entwickler nutzen Tests, um Fehler früh zu erkennen, die Softwarequalität zu sichern und zukünftige Änderungen abzusichern.

## How to:

Kotlin bietet eine gute Integration mit JUnit, einer beliebten Java-Testbibliothek. Hier ein einfaches Beispiel, wie ein Test in Kotlin aussieht:

```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class MathUtilsTest {

    @Test
    fun testAddition() {
        assertEquals(4, MathUtils.add(2, 2))
    }
}

object MathUtils {
    fun add(a: Int, b: Int): Int = a + b
}
```

Dieser Test überprüft die `add` Funktion der `MathUtils` Klasse. Der erwartete Output bei Ausführung des Tests ist:

```
Test passed.
```

## Deep Dive

Tests in Kotlin nutzen oft JUnit oder spezifische Kotlin-Test-Frameworks wie Spek und Kotest. Historisch kommt Testing aus der Entwicklergemeinschaft um besseren Code zu fördern. Andere Testarten umfassen Integrationstests, Systemtests und Akzeptanztests. Beim Testen geht es nicht nur darum, dass etwas aktuell funktioniert, sondern dass es unter allen erwarteten Bedingungen funktioniert.

## See Also

- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Spek Framework](https://www.spekframework.org/)
- [Kotest: Powerful, elegant Kotlin test framework](https://kotest.io/)
