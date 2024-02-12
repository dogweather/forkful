---
title:                "Виділення підрядків"
aliases:
- uk/cpp/extracting-substrings.md
date:                  2024-01-20T17:45:21.212693-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке та Навіщо?
Витягування підрядків – це процес отримання частини рядка. Програмісти роблять це, щоб обробляти, аналізувати чи модифікувати дані, що містяться в тексті.

## Як це робити:
```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Привіт, світ! Як справи?";
    std::string substring = fullString.substr(7, 5); // Витягуємо "світ"

    std::cout << "Повний рядок: " << fullString << "\n";
    std::cout << "Підрядок: " << substring << "\n";

    // Вивід:
    // Повний рядок: Привіт, світ! Як справи?
    // Підрядок: світ
}

```

## Поглиблений аналіз:
Підрядки в C++ можна витягувати з використанням методу `substr()` від кількості років. Історично є й інші способи, такі як використання C-стильних функцій типу `strncpy`. Однак, зі стандартними рядками в C++ (std::string), метод `substr()` став більш переважним через зручність і безпеку. Альтернативою може бути використання алгоритмів з бібліотеки `<algorithm>` або лямбда-функції для більш складних задач витягування.

## Дивись також:
- [cppreference.com, std::string::substr](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [cplusplus.com, String substring](http://www.cplusplus.com/reference/string/string/substr/)
- [Stack Overflow, C++ string slicing](https://stackoverflow.com/questions/4214314/string-slicing-in-c)
