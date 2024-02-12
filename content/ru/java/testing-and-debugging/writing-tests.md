---
title:                "Написание тестов"
aliases:
- /ru/java/writing-tests/
date:                  2024-01-29T00:06:01.692645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Написание тестов - это разработка кода, который проверяет, правильно ли работает другой код. Программисты делают это, чтобы заранее обнаруживать ошибки, гарантировать, что программное обеспечение работает как ожидается, и поддерживать качество кода со временем.

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
