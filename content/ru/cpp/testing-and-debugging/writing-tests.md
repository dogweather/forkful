---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:36.139327-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C \u043F\u0440\u043E\u0441\u0442\
  \u0443\u044E \u0444\u0443\u043D\u043A\u0446\u0438\u044E \u043D\u0430 C++ \u0438\
  \ \u0442\u0435\u0441\u0442 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u043D\u0438\u0435\u043C \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\
  \u043A\u0430 Catch2."
lastmod: '2024-03-13T22:44:45.614334-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C \u043F\u0440\u043E\u0441\u0442\u0443\u044E \u0444\
  \u0443\u043D\u043A\u0446\u0438\u044E \u043D\u0430 C++ \u0438 \u0442\u0435\u0441\u0442\
  \ \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435\u043C \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0430 Catch2."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как:
Давайте используем простую функцию на C++ и тест с использованием фреймворка Catch2.

```cpp
// main.cpp
#define CATCH_CONFIG_MAIN  // Позволим Catch предоставить main().
#include <catch2/catch.hpp>

int Add(int a, int b) {
    return a + b;
}

TEST_CASE( "Сложение работает", "[математика]" ) {
    REQUIRE( Add(2, 2) == 4 );
}
```
Скомпилируйте с помощью `g++ -std=c++17 main.cpp -o test -lcatch2` и запустите `./test`. Пример вывода:

```
Все тесты пройдены (1 утверждение в 1 тестовом примере)
```

## Подробнее
Тестирование всегда не было нормой. В 70-х это было ручное. Теперь автоматические тесты являются ключевыми в гибкой разработке и разработке через тестирование (TDD - Test-Driven Development). Альтернативы Catch2? Google Test, Boost.Test и CppUnit, каждый с уникальными особенностями. Помните: тесты проверяют, соответствует ли код требованиям, а не корректность этих требований — это вопрос спецификации.

## См. также
- Catch2: https://github.com/catchorg/Catch2
- Google Test: https://github.com/google/googletest
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- CppUnit: https://freedesktop.org/wiki/Software/cppunit/
