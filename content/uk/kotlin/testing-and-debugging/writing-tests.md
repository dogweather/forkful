---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:31.927994-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Kotlin \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0444\u0440\u0430\u0433\
  \u043C\u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u0434\u0443, \u044F\u043A\u0456\
  \ \u0430\u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0447\u043D\u043E \u043F\u0435\
  \u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C \u0444\u0443\u043D\u043A\u0446\
  \u0456\u043E\u043D\u0430\u043B\u044C\u043D\u0443 \u043F\u0440\u0430\u0432\u0438\u043B\
  \u044C\u043D\u0456\u0441\u0442\u044C \u0432\u0430\u0448\u0438\u0445 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043D\u0438\u0445 \u043C\u043E\u0434\u0443\u043B\u0456\
  \u0432,\u2026"
lastmod: '2024-03-11T00:14:23.079845-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Kotlin \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0444\u0440\u0430\u0433\
  \u043C\u0435\u043D\u0442\u0456\u0432 \u043A\u043E\u0434\u0443, \u044F\u043A\u0456\
  \ \u0430\u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0447\u043D\u043E \u043F\u0435\
  \u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C \u0444\u0443\u043D\u043A\u0446\
  \u0456\u043E\u043D\u0430\u043B\u044C\u043D\u0443 \u043F\u0440\u0430\u0432\u0438\u043B\
  \u044C\u043D\u0456\u0441\u0442\u044C \u0432\u0430\u0448\u0438\u0445 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043D\u0438\u0445 \u043C\u043E\u0434\u0443\u043B\u0456\
  \u0432,\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
---

{{< edit_this_page >}}

## Що та Чому?

Написання тестів на Kotlin включає створення фрагментів коду, які автоматично перевіряють функціональну правильність ваших програмних модулів, забезпечуючи їх роботу відповідно до очікувань. Програмісти роблять це для раннього виявлення помилок, полегшення рефакторингу коду і надання документації щодо того, як передбачається, що програмні компоненти будуть працювати.

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
