---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:33.739399-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: C++ \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0436\u0438\u0437\
  \u043D\u044C \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0437\u0430\u0433\
  \u043E\u043B\u043E\u0432\u043E\u0447\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\
  \u0430 `<chrono>`."
lastmod: '2024-03-13T22:44:45.630138-06:00'
model: gpt-4-0125-preview
summary: "C++ \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0436\u0438\u0437\u043D\
  \u044C \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0437\u0430\u0433\u043E\
  \u043B\u043E\u0432\u043E\u0447\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430\
  \ `<chrono>`."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

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
