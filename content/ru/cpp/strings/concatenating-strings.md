---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:40.781180-07:00
description: "\u041A\u0430\u043A: \u0412 C++ \u0443 \u043D\u0430\u0441 \u0435\u0441\
  \u0442\u044C \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\
  \u043E\u0441\u043E\u0431\u043E\u0432 \u043A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\
  \u0430\u0446\u0438\u0438 \u0441\u0442\u0440\u043E\u043A. \u0412\u043E\u0442 \u043F\
  \u0440\u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435\u043C `std::string` \u0438 \u043E\u043F\u0435\
  \u0440\u0430\u0442\u043E\u0440\u0430 \u0441\u043B\u043E\u0436\u0435\u043D\u0438\u044F\
  \ (`+`)."
lastmod: '2024-03-13T22:44:45.593550-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C++ \u0443 \u043D\u0430\u0441 \u0435\u0441\u0442\u044C \u043D\u0435\
  \u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\u043E\
  \u0432 \u043A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0438\u0438\
  \ \u0441\u0442\u0440\u043E\u043A."
title: "\u0421\u043A\u043B\u0435\u0438\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\
  \u043E\u043A"
weight: 3
---

## Как:
В C++ у нас есть несколько способов конкатенации строк. Вот пример с использованием `std::string` и оператора сложения (`+`):

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Привет, ";
    std::string world = "Мир!";
    
    std::string greeting = hello + world;
    
    std::cout << greeting << std::endl; // Выводит: Привет, Мир!
    return 0;
}
```

Быстро и просто, не правда ли? Но мы также можем использовать `append()`:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Привет, ";
    hello.append("Мир!");
    
    std::cout << hello << std::endl; // Выводит: Привет, Мир!
    return 0;
}
```

Или даже `operator+=`, если вам так удобнее:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Привет, ";
    hello += "Мир!";
    
    std::cout << hello << std::endl; // Выводит: Привет, Мир!
    return 0;
}
```

## Погружение в тему
Исторически C++ взял эстафету у C, который использовал массивы символов и функции вроде `strcat()` для работы со строками. Это было более запутанно и склонно к ошибкам.

Современный C++ улучшил ситуацию с `std::string`. Это безопаснее, проще для чтения и предлагает варианты. Если `std::string` вам не подходит, есть `std::stringstream` или даже `std::format` (начиная с C++20) для поклонников форматирования.

Под капотом конкатенация строк включает в себя выделение памяти и копирование. Если это делать неосторожно, это может ударить по производительности вашей программы как кирпич. Умные указатели и семантика перемещения облегчают некоторые из этих проблем.

Не забудем о альтернативах - библиотеки вроде Boost, или работа с UTF-8 через `std::string_view` для операций с нулевым копированием в современном C++.

## См. также
- C++ справочник по `std::string`: https://cplusplus.com/reference/string/string/
- Рабочий проект стандарта языка программирования C++: http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/n4861.pdf
- Узнать больше о `std::format`: https://en.cppreference.com/w/cpp/utility/format
- Документация библиотеки Boost: https://www.boost.org/doc/libs/1_75_0/libs/string_algo/doc/html/index.html
