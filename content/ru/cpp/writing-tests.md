---
title:                "Написание тестов"
aliases:
- ru/cpp/writing-tests.md
date:                  2024-01-29T00:05:36.139327-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов проверяет, делает ли ваш код то, что он должен делать, что позволяет заранее выявлять ошибки. Программисты тестируют код, чтобы сэкономить время, избежать головной боли и обеспечить надежность.

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
