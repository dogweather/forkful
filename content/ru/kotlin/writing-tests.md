---
title:                "Написание тестов"
date:                  2024-01-29T00:06:12.894463-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов означает создание кода для проверки правильности работы другого кода. Программисты делают это, чтобы заранее выявить ошибки, сэкономить время и обеспечить постоянное соответствие программного обеспечения его предназначению.

## Как это сделать:
Для тестирования Kotlin использует JUnit. Вот как написать и запустить простой тест:

```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class CalculatorTest {
    
    @Test
    fun `добавляет два числа`() {
        assertEquals(4, Calculator.add(2, 2))
    }
}

object Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

Запустите его. Если ваш вывод выглядит так, вы на верном пути:

```
Тест пройден
```

## Погружение
JUnit, основная система для тестирования в Kotlin, имеет корни в Java. Альтернативные системы тестирования включают Spek и Kotest, каждая из которых имеет свои особенности синтаксиса и функционала. Написание тестов часто включает в себя понимание структуры СУТ (Система, Подлежащая Тестированию), имитацию зависимостей с помощью MockK или аналогичного и знание различий между модульным, интеграционным и функциональным тестированием.

## См. также
- Руководство пользователя JUnit 5: [junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Библиотека MockK: [mockk.io](https://mockk.io)
- Фреймворк Spek: [spekframework.org](https://spekframework.org)
- Kotest: [kotest.io](https://kotest.io)
