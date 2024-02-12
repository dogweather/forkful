---
title:                "Skriva tester"
aliases: - /sv/kotlin/writing-tests.md
date:                  2024-02-03T19:31:16.734948-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Kotlin innebär att man skapar kodsnuttar som automatiskt validerar den funktionella korrektheten i dina mjukvarumoduler, för att säkerställa att de fungerar som förväntat. Programmerare gör detta för att fånga upp buggar tidigt, underlätta kodrefaktorering och tillhandahålla dokumentation om hur mjukvarukomponenter är avsedda att fungera.

## Hur man gör:

Kotlin stöder testdriven utveckling med olika ramverk, där de mest populära är JUnit, Kotest och MockK för att skapa mockobjekt. Här är ett enkelt exempel med JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `lägger till två nummer`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Exempelutdata**

```text
Test passed.
```

För en mer sofistikerad testansats med Kotest, som erbjuder en mer idiomatisk Kotlin-testskrivningsstil, se exempel nedan:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "att lägga till 2 och 3 ska returnera 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Använda MockK för testning med mockobjekt:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data returnerar mockad data`() {
        every { repository.getData() } returns "Mockad Data"

        val result = service.getData()

        assertEquals("Mockad Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Exempelutdata**

```text
Test passed.
```

Dessa exempel illustrerar grunderna för att skriva enhetstester i Kotlin. När din applikation växer, överväg att utforska mer avancerade testtekniker och verktyg som tillhandahålls av varje ramverk.
