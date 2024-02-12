---
title:                "Интерполяция строки"
aliases:
- /ru/cpp/interpolating-a-string/
date:                  2024-01-28T23:59:04.020363-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Интерполяция строк заключается во вставке переменных в строки. Мы делаем это для создания сообщений на лету, персонализации вывода или построения динамических запросов.

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
