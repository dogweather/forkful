---
title:                "Преобразование строки в верхний регистр"
date:                  2024-01-28T23:55:34.451940-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Преобразование строки в верхний регистр подразумевает изменение всех символов в тексте на заглавные. Программисты делают это ради единообразия, акцентирования внимания или иногда для соответствия определенным стандартам данных.

## Как:
C++ предлагает различные способы преобразования строки в верхний регистр, но вот простой пример:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

std::string capitalizeString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
}

int main() {
    std::string text = "Hello, World!";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl;
    return 0;
}
```

Пример вывода:
```
HELLO, WORLD!
```

## Глубокое погружение
Чтобы сделать все символы строки заглавными в C++, исторически мы использовали цикл для итерации по каждому символу, применяя функцию `toupper` из `<cctype>`.

По мере развития C++, Стандартная Библиотека Шаблонов (STL) предоставила такие алгоритмы, как `std::transform`, которые могут применять функцию ко всей последовательности. Этот стиль способствует более чистому коду и потенциально лучшей производительности благодаря алгоритмическим оптимизациям.

Помимо `std::transform`, есть также возможность использования диапазонов (начиная с C++20), что делает код еще более кратким и выразительным. Но это тема для другого разговора.

Альтернативы преобразования строк включают написание собственной функции или использование внешних библиотек, например, Boost. Все зависит от того, сколько контроля вам нужно и какие зависимости вы готовы принять.

При использовании `std::transform` помните, что она напрямую модифицирует строку. Если важно сохранить исходный регистр строки, всегда работайте с копией.

## Смотрите также
- Справочник C++ по `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Справочник C++ по `toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- Обзор диапазонов C++20: https://en.cppreference.com/w/cpp/ranges