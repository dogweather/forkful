---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:08.123772-07:00
description: "Hvordan: Kotlin st\xF8tter testdrevet utvikling med forskjellige rammeverk,\
  \ der de mest popul\xE6re er JUnit, Kotest og MockK for mocking. Her er et enkelt\u2026"
lastmod: '2024-03-13T22:44:40.754188-06:00'
model: gpt-4-0125-preview
summary: "Kotlin st\xF8tter testdrevet utvikling med forskjellige rammeverk, der de\
  \ mest popul\xE6re er JUnit, Kotest og MockK for mocking."
title: Skrive tester
weight: 36
---

## Hvordan:
Kotlin støtter testdrevet utvikling med forskjellige rammeverk, der de mest populære er JUnit, Kotest og MockK for mocking. Her er et enkelt eksempel som bruker JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `legger til to tall`() {
        val calculator = Calculator()
        val resultat = calculator.add(2, 3)
        assertEquals(5, resultat)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Eksempelresultat**

```text
Testen bestått.
```

For en mer sofistikert testtilnærming med Kotest, som tilbyr en mer idiomatisk Kotlin testskrivingsstil, se eksempelet nedenfor:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "å legge til 2 og 3 skal returnere 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Bruk av MockK for testing med mock-objekter:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `hent data returnerer mocket data`() {
        every { repository.getData() } returns "Mocket Data"

        val resultat = service.getData()

        assertEquals("Mocket Data", resultat)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Eksempelresultat**

```text
Testen bestått.
```

Disse eksemplene illustrerer grunnleggende prinsipper for å skrive enhetstester i Kotlin. Ettersom applikasjonen din vokser, bør du vurdere å utforske mer avanserte testteknikker og verktøy som hvert rammeverk tilbyr.
