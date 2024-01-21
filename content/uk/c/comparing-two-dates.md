---
title:                "Порівняння двох дат"
date:                  2024-01-20T17:32:40.574863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Що & Чому?)
Порівняння двох дат у програмуванні -- це процес визначення їх відношення: яка дата передує, настає після або збігається з іншою. Програмісти роблять це, щоб управляти подіями, термінами та періодами.

## How to: (Як це зробити:)
```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0, 0, 0, 12, 3, 121}; // 12 квітня 2021
    struct tm date2 = {0, 0, 0, 15, 7, 122}; // 15 серпня 2022
    
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    if (time1 < time2)
        printf("Дата1 передує Даті2\n");
    else if (time1 > time2)
        printf("Дата1 настає після Дати2\n");
    else
        printf("Дати збігаються\n");

    return 0;
}
```
Вивід:
```
Дата1 передує Даті2
```

## Deep Dive (Поглиблено:)
Порівняти дві дати в C можна через структуру `tm`, яка датується з 1989 року зі стандарту ANSI C. В цьому контексті, `mktime` перетворює `tm` в `time_t`, який представляє час в секундах, що пройшли з 00:00:00 UTC 1 січня 1970 (epoch). Оскільки `time_t` – звичайний числовий тип, порівняння стає легшим.

Іншим варіантом є використання бібліотек як `<chrono>` в сучасному C++ або сторонніх бібліотек, що надають більш гнучкі інструменти для роботи з датами і часом.

Імплементаційні деталі важливі, зокрема у високосні роки, різні часові пояси та врахування секунд переходу, де стандартна бібліотека може мати обмеження.

## See Also (Дивіться також):
- ISO C стандарт (ISO/IEC 9899) для деталей структури tm та time.h: https://www.iso.org/standard/74528.html
- C++ Date and Time tutorial (якщо ви віддаєте перевагу C++): http://www.cplusplus.com/reference/ctime/
- POSIX strftime для форматування дат: https://pubs.opengroup.org/onlinepubs/007908775/xsh/strftime.html