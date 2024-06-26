---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:34.384221-07:00
description: "\u042F\u043A: C++ \u043D\u0430\u0434\u0430\u0454 \u043A\u0456\u043B\u044C\
  \u043A\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u043E\u0442\u0440\
  \u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457\
  \ \u0434\u0430\u0442\u0438, \u0432\u043A\u043B\u044E\u0447\u0430\u044E\u0447\u0438\
  \ \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0443 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0443 C++ \u0442\u0430 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  , \u044F\u043A-\u043E\u0442 Boost. \u041D\u0430\u0441\u0442\u0443\u043F\u043D\u0456\
  \ \u043F\u0440\u0438\u043A\u043B\u0430\u0434\u0438\u2026"
lastmod: '2024-03-13T22:44:49.863410-06:00'
model: gpt-4-0125-preview
summary: "C++ \u043D\u0430\u0434\u0430\u0454 \u043A\u0456\u043B\u044C\u043A\u0430\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u043E\u0442\u0440\u0438\u043C\
  \u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457 \u0434\
  \u0430\u0442\u0438, \u0432\u043A\u043B\u044E\u0447\u0430\u044E\u0447\u0438 \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0443 \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0443 C++ \u0442\u0430 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u044F\
  \u043A-\u043E\u0442 Boost."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як:
C++ надає кілька способів отримання поточної дати, включаючи стандартну бібліотеку C++ та сторонні бібліотеки, як-от Boost. Наступні приклади демонструють, як виконати це завдання.

### Використовуючи `<chrono>` (C++20 та новіше)
C++20 ввів додаткові функції в бібліотеку `<chrono>`, що робить отримання поточної дати простим:
```cpp
#include <iostream>
#include <chrono>
#include <format> // для std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Захоплення поточного часу
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Конвертація в time_t

    // Форматування часу до зрозумілого формату
    std::cout << "Поточна Дата: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Приклад Виводу:**
```plaintext
Поточна Дата: 2023-03-15
```

### Використовуючи `<ctime>`
Для програмістів, які працюють зі старими версіями C++ або тих, хто віддає перевагу традиційній бібліотеці C:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Отримання поточного часу
    std::tm* now = std::localtime(&t);
    std::cout << "Поточна Дата: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Приклад Виводу:**
```plaintext
Поточна Дата: 2023-03-15
```

### Використовуючи Boost Date_Time
Для проектів, які використовують бібліотеки Boost, бібліотека Boost Date_Time пропонує альтернативний метод отримання поточної дати:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Отримання поточного дня за допомогою григоріанського календаря Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Поточна Дата: " << today << std::endl;

    return 0;
}
```
**Приклад Виводу:**
```plaintext
Поточна Дата: 2023-Mar-15
```
Ці приклади надають базовий фундамент для роботи з датами в C++, що є критично важливим для широкого спектру застосунків.
