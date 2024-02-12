---
title:                "Отримання поточної дати"
aliases:
- /uk/cpp/getting-the-current-date/
date:                  2024-02-03T19:09:34.384221-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Отримання поточної дати в C++ є фундаментальним завданням для програм, яким необхідно обробляти або відображати дати згідно з системним годинником. Це важливо для ведення журналів, маркування часу, планування завдань та будь-якої функціональності, що базується на датах і часі.

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
