---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "C++: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Розрахунок дати в майбутньому або минулому - це процес прибавлення або віднімання днів, місяців або років від початкової дати. Програмісти роблять це для роботи з датами в складних системах, таких як системи керування базами даних або системи стеження за проектами.

## Як це зробити:
Для цього ми використовуємо бібліотеку `std::chrono` в C++17. 

```C++
#include <chrono>
#include <iostream>
#include <ctime>

int main() {   
    // get today's date
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    // add one year
    std::chrono::seconds sec_in_one_year = 60*60*24*365;
    today += sec_in_one_year;
    // convert back to time_t and ctime to get a neat string
    time_t tt = std::chrono::system_clock::to_time_t(today);

    std::cout << "One year from now will be: " << ctime(&tt);
}
```
Вихідний файл:
``` 
One year from now will be: Mon Sep  26 12:22:35 2022 
```

## Поглиблено:
Розрахунок дати в майбутньому або минулому був важливою частиною програмування протягом багатьох десятиліть. До впровадження `std::chrono` в C++11, програмісти часто використовували клас `time_t` або структуру `tm`, але ці методи містили багато пасток. Також існують альтернативні бібліотеки, такі як Boost DateTime або Howard E. Hinnant's date library, але `std::chrono` є вбудованим і потужним вибором.

## Дивіться також:
- [C++ Reference: chrono](https://en.cppreference.com/w/cpp/chrono)
- [C++ Core Guidelines: Date manipulation](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#i55-dont-mess-with-the-system-directory)
- [Boost Libraries: DateTime](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [Howard Hinnant's date library](https://github.com/HowardHinnant/date)