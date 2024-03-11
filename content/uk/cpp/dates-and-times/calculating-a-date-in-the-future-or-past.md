---
date: 2024-01-20 17:31:11.112680-07:00
description: "\u0420\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043E\u043A \u0434\u0430\
  \u0442\u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\
  \u0443 \u0447\u0438 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u2014 \u0446\
  \u0435 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u0430\
  \u0442\u0438, \u044F\u043A\u0430 \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u0454\
  \u0442\u044C\u0441\u044F \u0434\u043E \u0447\u0438 \u043F\u0456\u0441\u043B\u044F\
  \ \u0437\u0430\u0434\u0430\u043D\u043E\u0457 \u0432\u0456\u0434\u043B\u0456\u043A\
  \u043E\u0432\u043E\u0457 \u0442\u043E\u0447\u043A\u0438. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438\
  \u2026"
lastmod: '2024-03-11T00:14:23.682668-06:00'
model: gpt-4-1106-preview
summary: "\u0420\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043E\u043A \u0434\u0430\
  \u0442\u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\
  \u0443 \u0447\u0438 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u2014 \u0446\
  \u0435 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u0430\
  \u0442\u0438, \u044F\u043A\u0430 \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u0454\
  \u0442\u044C\u0441\u044F \u0434\u043E \u0447\u0438 \u043F\u0456\u0441\u043B\u044F\
  \ \u0437\u0430\u0434\u0430\u043D\u043E\u0457 \u0432\u0456\u0434\u043B\u0456\u043A\
  \u043E\u0432\u043E\u0457 \u0442\u043E\u0447\u043A\u0438. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438\
  \u2026"
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
---

{{< edit_this_page >}}

## Що та Чому?
Розрахунок дати у майбутньому чи минулому — це визначення дати, яка відбувається до чи після заданої відлікової точки. Програмісти це роблять для обробки термінів, планування подій та дотримання дедлайнів.

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
