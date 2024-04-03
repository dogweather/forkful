---
date: 2024-01-20 17:45:21.212693-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: ."
lastmod: '2024-03-13T22:44:49.823116-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

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
