---
title:                "Сравнение двух дат"
aliases:
- ru/cpp/comparing-two-dates.md
date:                  2024-01-28T23:55:33.739399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Сравнение дат заключается в определении, какая из двух дат раньше, позже или если они одинаковы. Программисты делают это для организации событий, истечения сроков акций, планирования, напоминаний — короче говоря, для всего, что связано с временем.

## Как это сделать:
C++ упрощает жизнь с помощью заголовочного файла `<chrono>`.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    // Создаем точки времени system_clock
    system_clock::time_point today = system_clock::now();
    system_clock::time_point someDay = system_clock::now() - hours(24); // Вчера

    // Конвертируем в time_t для сравнения
    time_t today_time_t = system_clock::to_time_t(today);
    time_t someDay_time_t = system_clock::to_time_t(someDay);

    if (today_time_t > someDay_time_t) {
        std::cout << "Today is after someDay.\n";
    } else if (today_time_t < someDay_time_t) {
        std::cout << "Today is before someDay.\n";
    } else {
        std::cout << "Dates are the same.\n";
    }

    return 0;
}
```

Пример вывода:

```
Сегодня после некоторого дня.
```

## Углубленный взгляд:
С C++11 `<chrono>` стало тем местом, где всё происходит для даты и времени. До этого скорее всего приходилось иметь дело с `<ctime>` и структурами вроде `tm`. Не самый удобный вариант.

Альтернативы? Конечно, есть сторонние библиотеки вроде Boost.DateTime. Но зачем усложнять, когда есть `<chrono>`, который постоянно развивается.

Детали реализации, которые стоит иметь под рукой:
- `std::chrono` работает с точками времени и продолжительностями.
- `system_clock` измеряет реальное время.
- `time_point` - это конкретная точка во времени (например, дата).
- `time_t` - арифметический тип, удобный для сравнений.

## Смотрите также:
- Справочник C++ для `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Сравнение библиотек даты и времени: http://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html
- Хороший старый `<ctime>`, если вы чувствуете ностальгию или мазохизм: https://en.cppreference.com/w/cpp/header/ctime
