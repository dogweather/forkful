---
date: 2024-01-20 17:31:11.112680-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0436\u0435\
  \ \u0434\u0435\u043A\u0456\u043B\u044C\u043A\u0430 \u0434\u0435\u0441\u044F\u0442\
  \u0438\u043B\u0456\u0442\u044C \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0442\u044C \u0440\u0456\u0437\u043D\u0456 \u043C\u0435\u0442\
  \u043E\u0434\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\u0456\u043F\u0443\u043B\
  \u044F\u0446\u0456\u0439 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438. \u0412 \u0456\
  \u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E\u043C\u0443 \u043A\u043E\u043D\u0442\
  \u0435\u043A\u0441\u0442\u0456, \u043D\u0430\u0439\u043F\u043E\u043F\u0443\u043B\
  \u044F\u0440\u043D\u0456\u0448\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438\u2026"
lastmod: '2024-04-05T22:51:02.802024-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\
  \u0436\u0435 \u0434\u0435\u043A\u0456\u043B\u044C\u043A\u0430 \u0434\u0435\u0441\
  \u044F\u0442\u0438\u043B\u0456\u0442\u044C \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0440\u0456\u0437\u043D\u0456 \u043C\
  \u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044F\u0446\u0456\u0439 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438\
  ."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це робити:
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;
    
    // Сьогоднішня дата
    system_clock::time_point today = system_clock::now();
    
    // Розрахунок дати через 10 днів
    system_clock::time_point future_date = today + days(10);
    
    // Конвертація у стандартний час
    std::time_t future_time = system_clock::to_time_t(future_date);
    
    // Вивід
    std::cout << "Today is: " << std::ctime(&future_time);
    std::cout << "Date after 10 days will be: " << std::ctime(&future_time);

    // Розрахунок дати 10 днів назад
    system_clock::time_point past_date = today - days(10);
    std::time_t past_time = system_clock::to_time_t(past_date);
    std::cout << "Date 10 days ago was: " << std::ctime(&past_time);
    
    return 0;
}
```
Можливий вивід:
```
Today is: Thu Mar 4 2021
Date after 10 days will be: Sun Mar 14 2021
Date 10 days ago was: Tue Feb 22 2021
```

## Поглиблено:
Програмісти вже декілька десятиліть використовують різні методи для маніпуляцій з датами. В історичному контексті, найпопулярніші бібліотеки для C++ це `ctime` та більш сучасна `chrono`, яка з'явилась у C++11 і була поліпшена в наступних версіях стандарту.

Альтернативою є користування зовнішніми бібліотеками як-от Boost.Date_Time. Вони надають додаткову гнучкість, але для більшості завдань стандартної бібліотеки цілком достатньо.

Деталі реалізації включають обробку часових зон та перехід на літній/зимовий час. Важливо враховувати ці аспекти, коли додаток взаємодіє з користувачами у різних часових зонах.

## Дивіться також:
- [C++ `<chrono>` documentation](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)
- [`<ctime>` documentation](https://en.cppreference.com/w/cpp/header/ctime)
