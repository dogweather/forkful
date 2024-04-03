---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:08.937581-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: C++ \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\
  \u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \u043E\u0432 \u043F\u043E\u0438\u0441\u043A\u0430 \u0438 \u0437\u0430\u043C\u0435\
  \u043D\u044B \u0442\u0435\u043A\u0441\u0442\u0430. \u041D\u0438\u0436\u0435 \u043F\
  \u0440\u0438\u0432\u0435\u0434\u0435\u043D \u043F\u0440\u0438\u043C\u0435\u0440\
  \ \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435\u043C `std::string::find` \u0438\u2026"
lastmod: '2024-03-13T22:44:45.580800-06:00'
model: gpt-4-0125-preview
summary: "C++ \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\u0435\
  \u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\u043E\
  \u0432 \u043F\u043E\u0438\u0441\u043A\u0430 \u0438 \u0437\u0430\u043C\u0435\u043D\
  \u044B \u0442\u0435\u043A\u0441\u0442\u0430."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

## Как это сделать:
C++ предлагает несколько способов поиска и замены текста. Ниже приведен пример с использованием `std::string::find` и `std::string::replace`.

```cpp
#include <iostream>
#include <string>

int main() {
    std::string myText = "The quick brown fox jumps over the lazy dog.";
    std::string wordToSearch = "lazy";
    std::string replacement = "energetic";

    size_t pos = myText.find(wordToSearch);

    if (pos != std::string::npos) {
        myText.replace(pos, wordToSearch.length(), replacement);
    }

    std::cout << myText << std::endl; // Вывод: The quick brown fox jumps over the energetic dog.
    return 0;
}
```

## Подробнее
Функции `find` и `replace` уже давно являются частью класса `std::string` в C++, что делает их базовыми, но мощными средствами для манипуляций с текстом. До появления `std::string`, программисты на языке C использовали массивы символов и функции вроде `strstr` и `strcpy` из стандартной библиотеки C для аналогичных задач, что было более подвержено ошибкам и требовало ручного управления памятью.

Что касается альтернатив, другие компоненты стандартной библиотеки, такие как `std::regex`, предоставляют возможности манипуляции текстом на основе шаблонов для сложных сценариев поиска и замены. Сторонние библиотеки вроде Boost предлагают ещё более сложные варианты обработки текста.

Внутренне, поиск и замена включают в себя алгоритмы, которые итерируют по строке для нахождения соответствующих последовательностей символов и затем изменяют содержание строки соответственно. Эффективность этих операций может варьироваться в зависимости от имплементации и сложности поискового шаблона.

## См. также
- Справочник по C++ для `std::string::find`: https://en.cppreference.com/w/cpp/string/basic_string/find
- Справочник по C++ для `std::string::replace`: https://en.cppreference.com/w/cpp/string/basic_string/replace
- Справочник по регулярным выражениям в C++ `std::regex`: https://en.cppreference.com/w/cpp/regex
- Библиотека алгоритмов работы со строками Boost: https://www.boost.org/doc/libs/release/libs/algorithm/string/
