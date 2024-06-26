---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:01.692645-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\
  \u0430\u043F\u0438\u0448\u0435\u043C \u043F\u0440\u043E\u0441\u0442\u043E\u0439\
  \ \u0442\u0435\u0441\u0442 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u043D\u0438\u0435\u043C JUnit, \u043F\u043E\u043F\u0443\u043B\u044F\
  \u0440\u043D\u043E\u0433\u043E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\
  \u0430 \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u044F \u0432 Java. \u041C\u044B \u0431\u0443\u0434\u0435\u043C \u0442\
  \u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u043C\u0435\u0442\u043E\
  \u0434, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0441\u043A\u043B\u0430\u0434\
  \u044B\u0432\u0430\u0435\u0442\u2026"
lastmod: '2024-03-13T22:44:44.827463-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u043F\u0438\u0448\
  \u0435\u043C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0442\u0435\u0441\u0442\
  \ \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435\u043C JUnit, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u043E\u0433\
  \u043E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0430 \u0434\u043B\
  \u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\
  \ \u0432 Java."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как:
Давайте напишем простой тест с использованием JUnit, популярного фреймворка для тестирования в Java. Мы будем тестировать метод, который складывает два целых числа.

```java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 должно быть равно 5");
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Если метод работает, тест проходит без замечаний. Если он не проходит, JUnit выводит ошибку:

```
org.opentest4j.AssertionFailedError: 2 + 3 должно быть равно 5 ==> ожидалось: <5> но было: <4>
```

## Погружение в тему
Тестирование не всегда было приоритетом для программистов — оно получило распространение с развитием Agile и практиками, такими как Разработка через Тестирование (TDD). Альтернативы JUnit включают TestNG и Spock, каждый со своими преимуществами. Реализация хороших тестов — это искусство; это обычно включает имитацию зависимостей, соблюдение шаблонов тестирования и непрерывную интеграцию тестов в процесс сборки.

## Смотрите также
- Руководство пользователя JUnit 5: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Статья о Разработке через Тестирование: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
- Фреймворки для имитации: Mockito [https://site.mockito.org/](https://site.mockito.org/)
