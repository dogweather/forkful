---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester betyr å lage kode for å sjekke at annen kode virker som forventet. Programmerere gjør dette for å oppdage feil tidlig, forbedre kvaliteten og gjøre koden lettere å vedlikeholde.

## How to:
```Kotlin
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class CalculatorTest {

    @Test
    fun `add two numbers`() {
        val calculator = Calculator()
        assertEquals(4, calculator.add(2, 2))
    }
}

class Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

Sample Output:
```
Test passed: add two numbers
```

## Deep Dive
Testing i Kotlin startet med JUnit, en av de første rammeverkene for enhetstesting i Java. Alternativer inkluderer Spek og Kotest. Viktig i implementeringen er isolasjon av tester, reproduserbarhet og automatisering gjennom CI/CD-pipelines.

## See Also
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Kotest Framework](https://github.com/kotest/kotest)