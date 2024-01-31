---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:11.112680-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/calculating-a-date-in-the-future-or-past.md"
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
