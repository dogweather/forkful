---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:12.435541-07:00
description: "Jak to zrobi\u0107: Kotlin wspiera rozw\xF3j sterowany testami z wykorzystaniem\
  \ r\xF3\u017Cnych framework\xF3w, z kt\xF3rych najpopularniejsze to JUnit, Kotest\
  \ i MockK do\u2026"
lastmod: '2024-03-13T22:44:35.368457-06:00'
model: gpt-4-0125-preview
summary: "Kotlin wspiera rozw\xF3j sterowany testami z wykorzystaniem r\xF3\u017C\
  nych framework\xF3w, z kt\xF3rych najpopularniejsze to JUnit, Kotest i MockK do\
  \ tworzenia mock\xF3w."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Kotlin wspiera rozwój sterowany testami z wykorzystaniem różnych frameworków, z których najpopularniejsze to JUnit, Kotest i MockK do tworzenia mocków. Oto prosty przykład użycia JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `dodaje dwie liczby`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Przykładowy Wynik**

```text
Test zakończony powodzeniem.
```

Dla bardziej zaawansowanego podejścia do testowania z wykorzystaniem Kotest, który oferuje bardziej idiomatyczny styl pisania testów w Kotlinie, zobacz poniższy przykład:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "dodanie 2 i 3 powinno zwrócić 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Użycie MockK do testowania z mockami:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data zwraca zmockowane dane`() {
        every { repository.getData() } returns "Mocked Data"

        val result = service.getData()

        assertEquals("Mocked Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Przykładowy Wynik**

```text
Test zakończony powodzeniem.
```

Te przykłady ilustrują podstawy pisania testów jednostkowych w Kotlinie. W miarę wzrostu aplikacji warto rozważyć badanie bardziej zaawansowanych technik testowania oraz narzędzi oferowanych przez każdy framework.
