---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:31.927994-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: Kotlin\
  \ \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u043E\u0437\u0440\
  \u043E\u0431\u043A\u0443, \u043E\u0440\u0456\u0454\u043D\u0442\u043E\u0432\u0430\
  \u043D\u0443 \u043D\u0430 \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\
  \u044F, \u0437 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\
  \u044F\u043C \u0440\u0456\u0437\u043D\u0438\u0445 \u0444\u0440\u0435\u0439\u043C\
  \u0432\u043E\u0440\u043A\u0456\u0432, \u043D\u0430\u0439\u043F\u043E\u043F\u0443\
  \u043B\u044F\u0440\u043D\u0456\u0448\u0438\u043C\u0438 \u0437 \u044F\u043A\u0438\
  \u0445 \u0454 JUnit, Kotest \u0442\u0430 MockK\u2026"
lastmod: '2024-03-13T22:44:49.225106-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u043E\
  \u0437\u0440\u043E\u0431\u043A\u0443, \u043E\u0440\u0456\u0454\u043D\u0442\u043E\
  \u0432\u0430\u043D\u0443 \u043D\u0430 \u0442\u0435\u0441\u0442\u0443\u0432\u0430\
  \u043D\u043D\u044F, \u0437 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\
  \u043D\u043D\u044F\u043C \u0440\u0456\u0437\u043D\u0438\u0445 \u0444\u0440\u0435\
  \u0439\u043C\u0432\u043E\u0440\u043A\u0456\u0432, \u043D\u0430\u0439\u043F\u043E\
  \u043F\u0443\u043B\u044F\u0440\u043D\u0456\u0448\u0438\u043C\u0438 \u0437 \u044F\
  \u043A\u0438\u0445 \u0454 JUnit, Kotest \u0442\u0430 MockK \u0434\u043B\u044F \u043C\
  \u043E\u043A\u0443\u0432\u0430\u043D\u043D\u044F."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

## Як це робити:
Kotlin підтримує розробку, орієнтовану на тестування, з використанням різних фреймворків, найпопулярнішими з яких є JUnit, Kotest та MockK для мокування. Ось простий приклад з використанням JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `adds two numbers`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Приклад виводу**

```text
Тест пройдено.
```

Для більш складного підходу до тестування з використанням Kotest, який пропонує стиль написання тестів, більш ідіоматичний для Kotlin, дивіться наведений нижче приклад:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "додавання 2 і 3 повинно повернути 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Використання MockK для тестування з моками:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `отримання даних повертає замоковані дані`() {
        every { repository.getData() } returns "Замоковані Дані"

        val result = service.getData()

        assertEquals("Замоковані Дані", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Приклад виводу**

```text
Тест пройдено.
```

Ці приклади ілюструють основи написання модульних тестів на Kotlin. По мірі зростання вашого додатку, розгляньте можливість дослідження більш передових технік тестування та інструментів, які надаються кожним фреймворком.
