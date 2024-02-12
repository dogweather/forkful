---
title:                "Письмо тестів"
aliases:
- /uk/kotlin/writing-tests/
date:                  2024-02-03T19:31:31.927994-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
