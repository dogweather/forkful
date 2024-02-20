---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:12.435541-07:00
description: "Pisanie test\xF3w w Kotlinie polega na tworzeniu fragment\xF3w kodu,\
  \ kt\xF3re automatycznie weryfikuj\u0105 poprawno\u015B\u0107 funkcjonaln\u0105\
  \ modu\u0142\xF3w oprogramowania, upewniaj\u0105c\u2026"
lastmod: 2024-02-19 22:04:54.499697
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Kotlinie polega na tworzeniu fragment\xF3w kodu, kt\xF3\
  re automatycznie weryfikuj\u0105 poprawno\u015B\u0107 funkcjonaln\u0105 modu\u0142\
  \xF3w oprogramowania, upewniaj\u0105c\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pisanie testów w Kotlinie polega na tworzeniu fragmentów kodu, które automatycznie weryfikują poprawność funkcjonalną modułów oprogramowania, upewniając się, że działają one zgodnie z oczekiwaniami. Programiści robią to, by wcześnie wykrywać błędy, ułatwić refaktoryzację kodu oraz dostarczyć dokumentację na temat zamierzonego działania komponentów oprogramowania.

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
