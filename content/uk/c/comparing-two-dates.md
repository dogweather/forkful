---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що & Чому?
Порівняння двох дат - це процес встановлення, яка з двох дат відбулася раніше, чи вони співпадають. Програмісти роблять це для організації подій, відслідковування часу або розрахунку віку.

## Як це робиться:
```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm date1 = {0}, date2 = {0};
    // 2022-11-14
    date1.tm_year = 2022 - 1900;
    date1.tm_mon = 11 - 1;
    date1.tm_mday = 14;
    // 2023-11-14
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 11 - 1;
    date2.tm_mday = 14;

    double seconds = difftime(mktime(&date2), mktime(&date1));
    printf("Дні між датами: %.f\n", seconds / (60 * 60 * 24));
    return 0;
}
```
Вихідне значення:
```
Дні між датами: 365
```

## Поглиблений погляд
1. **Історичний контекст**: Функція `difftime` була вперше внесена у стандарт ISO C в 1989 році, щоб полегшити порівняння значень часових змінних в форматі `time_t`.
2. **Альтернативи**: Для більш складних операцій, таких як розрахунок кількість днів у високосному році, можна використовувати бібліотеки, такі як `Boost.Date_Time` або `Howard Hinnant's date library`.
3. **Деталі реалізації**: При порівнянні дат, ми конвертуємо структуру `struct tm` у значення `time_t` отриманим з функції `mktime`, надаючи нам можливість порівняти дати у форматі секунд з початку епохи Unix (це 00:00:00 UTC, 1 січня 1970 року).

## Дивіться також
1. [Стандарт ISO C (1989)](https://www.iso.org/standard/17782.html)
2. [Бібліотека Boost.Date_Time](https://www.boost.org/doc/libs/1_72_0/doc/html/date_time.html)
3. [Бібліотека дати Howard Hinnant](https://github.com/HowardHinnant/date)