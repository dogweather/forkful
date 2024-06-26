---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:12.894463-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\
  \u0430\u043D\u0438\u044F Kotlin \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442 JUnit. \u0412\u043E\u0442 \u043A\u0430\u043A \u043D\u0430\u043F\u0438\
  \u0441\u0430\u0442\u044C \u0438 \u0437\u0430\u043F\u0443\u0441\u0442\u0438\u0442\
  \u044C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0442\u0435\u0441\u0442."
lastmod: '2024-03-13T22:44:44.985221-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u044F Kotlin \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\
  \u0442 JUnit."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
