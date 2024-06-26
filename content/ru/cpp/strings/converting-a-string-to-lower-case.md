---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:31.161950-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0443\u0441\u0442\u0440\u0430\u043D\u0438\u0442\u044C \u0440\
  \u0430\u0437\u043B\u0438\u0447\u0438\u044F \u0432 \u0440\u0435\u0433\u0438\u0441\
  \u0442\u0440\u0435 \u0432 C++, \u0437\u0430\u0441\u0442\u0430\u0432\u043B\u044F\u044F\
  \ \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u044B\u0435 \u0431\u0443\u043A\u0432\
  \u044B \u0443\u0441\u0442\u0443\u043F\u0430\u0442\u044C \u043C\u0435\u0441\u0442\
  \u043E \u0441\u0442\u0440\u043E\u0447\u043D\u044B\u043C."
lastmod: '2024-03-13T22:44:45.584628-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u0443\u0441\u0442\u0440\u0430\u043D\u0438\u0442\u044C \u0440\u0430\
  \u0437\u043B\u0438\u0447\u0438\u044F \u0432 \u0440\u0435\u0433\u0438\u0441\u0442\
  \u0440\u0435 \u0432 C++, \u0437\u0430\u0441\u0442\u0430\u0432\u043B\u044F\u044F\
  \ \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u044B\u0435 \u0431\u0443\u043A\u0432\
  \u044B \u0443\u0441\u0442\u0443\u043F\u0430\u0442\u044C \u043C\u0435\u0441\u0442\
  \u043E \u0441\u0442\u0440\u043E\u0447\u043D\u044B\u043C."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это сделать:
Вот как вы можете устранить различия в регистре в C++, заставляя заглавные буквы уступать место строчным:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string origText = "C++ заставляет меня Кричать!";
    std::string lowerText = origText;

    std::transform(origText.begin(), origText.end(), lowerText.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    std::cout << "Оригинал: " << origText << std::endl;
    std::cout << "В нижнем регистре: " << lowerText << std::endl;
    
    return 0;
}
```
Вывод:
```
Оригинал: C++ заставляет меня Кричать!
В нижнем регистре: c++ заставляет меня кричать!
```

## Подробнее
Раньше, до появления `std::transform` и лямбда-выражений, программисты проходились циклом по каждому символу и вручную преобразовывали его в нижний регистр — это было немного более тяжелой работой. `std::transform` вместе с `std::tolower` работает эффективно и с меньшей вероятностью ошибок, хотя, учитывая C++, существуют и другие способы. Обратите внимание на локаль: поведение `std::tolower` может отличаться. Если ваш проект может использовать Unicode, обратите внимание на сторонние библиотеки вроде ICU, разработанные для глобальной сцены.

Также стоит упомянуть добавление в C++20, `std::ranges::transform`, которое приносит трансформации на основе диапазонов, обновляя синтаксис и придерживаясь философии "диапазона", согласно которой кодирование должно быть более интуитивным и менее подверженным ошибкам.

Что касается деталей реализации, каждый символ имеет значение ASCII, и разница между строчными и прописными буквами постоянна. Преобразования проверяют эти значения, чтобы перевести их в нижний регистр — в основном играя в числовое лимбо.

## Смотрите также
Для любопытных котов, жаждущих большего:

- Справочник C++ для `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Справочник C++ для `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Детали о `std::ranges` в C++20: https://en.cppreference.com/w/cpp/ranges

Хотите понять Unicode? Попробуйте ICU Project:
- ICU Project: http://site.icu-project.org/home
