---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:04.020363-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: C++ \u043D\u0435 \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\u0442\
  \u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u0438\u043D\u0442\u0435\u0440\u043F\u043E\
  \u043B\u044F\u0446\u0438\u0438 \u0441\u0442\u0440\u043E\u043A, \u043A\u0430\u043A\
  \ \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0435 \u0434\u0440\u0443\u0433\
  \u0438\u0435 \u044F\u0437\u044B\u043A\u0438. \u0427\u0430\u0441\u0442\u043E \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\u0441\u044F `std::ostringstream`,\
  \ `std::format` (\u043D\u0430\u0447\u0438\u043D\u0430\u044F \u0441\u2026"
lastmod: '2024-03-13T22:44:45.582629-06:00'
model: gpt-4-0125-preview
summary: "C++ \u043D\u0435 \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\u0442\u0440\
  \u043E\u0435\u043D\u043D\u043E\u0439 \u0438\u043D\u0442\u0435\u0440\u043F\u043E\u043B\
  \u044F\u0446\u0438\u0438 \u0441\u0442\u0440\u043E\u043A, \u043A\u0430\u043A \u043D\
  \u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0435 \u0434\u0440\u0443\u0433\u0438\u0435\
  \ \u044F\u0437\u044B\u043A\u0438."
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 8
---

## Как это сделать:
C++ не имеет встроенной интерполяции строк, как некоторые другие языки. Часто используются `std::ostringstream`, `std::format` (начиная с C++20) или форматирование в стиле printf.

С использованием `std::ostringstream`:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream message;
    int age = 30;
    message << "Привет, мне " << age << " лет.";
    std::cout << message.str() << std::endl; // "Привет, мне 30 лет."
}
```

С использованием `std::format` (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string message = std::format("Привет, мне {} лет.", age);
    std::cout << message << std::endl; // "Привет, мне 30 лет."
}
```

## Подробнее
До C++20 мы объединяли строки с помощью потоков или sprintf, что было неудобно. С появлением `std::format` мы догнали современные языки, такие как Python с их f-строками.

`std::ostringstream`: Это предоставляет нам способ построения строк похожий на работу с потоками. Это универсально, но не самый лаконичный метод. Его долго использовали, потому что он безопасен и прост в использовании.

`std::format`: Появился в C++20, предлагает форматирование похожее на Python. Это более читаемо и эффективно, чем конкатенация строк, но требует более новых компиляторов.

Существуют альтернативы, такие как Boost.Format или использование конкатенации строк, но они не такие чистые или могут привести к дополнительным затратам.

Интерполяция строк - это сахар, но он сладкий. Это упрощает код и избегает потерь производительности из-за многократного добавления строк.

## Смотрите также
- [cppreference по std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference по std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [Библиотека Boost.Format](https://www.boost.org/doc/libs/release/libs/format/)
