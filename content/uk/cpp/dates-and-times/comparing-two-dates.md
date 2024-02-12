---
title:                "Порівняння двох дат"
aliases:
- /uk/cpp/comparing-two-dates.md
date:                  2024-01-20T17:32:31.054040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат – це процедура визначення відносних позицій двох об'єктів у часі. Програмісти роблять це для управління подіями, збереження послідовності, та для функцій, які залежать від часу.

## Як це зробити:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    // Встановлення двох дат
    std::tm tm1 = {};
    std::tm tm2 = {};
    strptime("2023-03-15", "%Y-%m-%d", &tm1);
    strptime("2023-03-20", "%Y-%m-%d", &tm2);

    // Конвертація в time_point
    std::chrono::system_clock::time_point tp1 = std::chrono::system_clock::from_time_t(mktime(&tm1));
    std::chrono::system_clock::time_point tp2 = std::chrono::system_clock::from_time_t(mktime(&tm2));

    // Порівняння дат
    if (tp1 < tp2)
        std::cout << "Перша дата раніше другої." << std::endl;
    else if (tp1 > tp2)
        std::cout << "Перша дата пізніше другої." << std::endl;
    else
        std::cout << "Дати однакові." << std::endl;
    
    return 0;
}
```

Вивід:
```
Перша дата раніше другої.
```

## Поглиблений огляд:

Історія: Порівняння дат – стара проблема в комп'ютерних науках. Спочатку це було здійснено через операції зі строками або числами.

Альтернативи: Існують бібліотеки, як `boost::date_time` чи старі методи `std::mktime` і `std::difftime`, але `std::chrono` є засобом сучасного C++, який запроваджений для стандартної бібліотеки.

Деталі реалізації: `std::chrono` використовує точні часові точки (`time_points`) для представлення моментів у часі та тривалостей (`durations`). Це дозволяє зробити порівняння точними та безпечними для типів.

## Дивись також:

- C++ reference про `std::chrono`: https://en.cppreference.com/w/cpp/chrono
- Документація по `boost::date_time`: https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html
- Tutorial по роботі із датами і часом в C++: https://www.learncpp.com/cpp-tutorial/date-and-time/
