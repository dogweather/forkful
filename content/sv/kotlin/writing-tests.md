---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Testning är processen att verifiera att din kod gör vad den ska. Programmerare skriver tester för att säkerställa att programmet fungerar rätt och för att undvika framtida buggar vid uppdateringar.

## Hur gör man:
```kotlin
import org.junit.Test
import org.junit.Assert.*

class ExampleUnitTest {
    @Test
    fun addition_isCorrect() {
        assertEquals(4, 2 + 2)
    }

    @Test(expected = ArithmeticException::class)
    fun division_byZero() {
        val result = 2 / 0
    }
}
```
Köper du koden ser du något i stil med:
```
Test passed: addition_isCorrect
Test failed: division_byZero, expected: ArithmeticException
```

## Fördjupning
Testning i Kotlin görs ofta med JUnit, ett ramverk som funnits sedan 90-talet. Alternativ till JUnit inkluderar TestNG eller Kotest. Inom Kotlin använder man ofta Mockk för att mocka beroenden och JetBrains utvecklade Spek för BDD-style testning.

## Se även
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Kotest, a powerful Kotlin testing library](https://kotest.io/)
- [Mockk, mocking library for Kotlin](https://mockk.io/)
- [Spek Framework](https://www.spekframework.org/)
