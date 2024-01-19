---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "C: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#Комп'ютинг Дати: Минуле й Майбутнє

## Що і Навіщо?

Обчислення дат у майбутньому або минулому - це процес визначення точної дати, відносно поточного моменту, використовуючи різницю у даті, вимірювану в днях, тижнях, місяцях, або роках. Це важливо для програмістів, бо багато додатків, як в логістиці, банківських послугах і т.д., залежать від обчислення дат.

## Як це робиться?

В C ви можете використовувати бібліотеку `time.h` для роботи з датами. Спочатку включіть її у свій файл:

```C
#include <time.h>
```
Далі, ви можете задати будь-яку дату та час за допомогою структур `struct tm` та `time_t`. Наприклад, наступний код демонструє, как обчислити дату у майбутнє на 30 днів:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm my_time;
    time_t now;

    time(&now);
    my_time = *localtime(&now);

    printf("Current date and time: %s", ctime(&now));

    my_time.tm_mday += 30;
    mktime(&my_time);

    printf("Future date and time: %s", asctime(&my_time));
    
    return 0;
}
```

Вивід буде:

```
Current date and time: Tue Jun 1 13:55:42 2021
Future date and time: Thu Jul 1 13:55:42 2021
```

## В глибину

Працювати з датами в C не завжди було легко. Методики розрахунку часу були вебудовані в C з самого початку в бібліотеці `time.h`, яка була частиною стандартної бібліотеки C через її фундаментальну важливість.

Є альтернативи обчисленням дати в майбутнє або минулому в C. Ви можете використовувати зовнішні бібліотеки, наприклад `Boost` або `date.h`. Вони можуть надати більше можливостей та гнучкості, але вони аналогічно залежать від `time.h` та `struct tm`.

Що стосується деталей реалізації, то при обчисленні дат `struct tm` враховує переходи на літній і зимовий час, високосні роки і т.д., але важливо пам'ятати, що не всі системи реалізують ці розрахунки так, як ви очікуєте. Тому краще завжди тестувати свій код.

## Див. також

1. Онлайн C бібліотека. [time.h](https://www.cplusplus.com/reference/ctime/)
2. Boost бібліотека. [date_time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
3. С. Броуді "C Programming Language, 2nd Edition". [Chapter 7, Section 6 - Time of day and conversion functions](https://en.wikisource.org/wiki/The_C_Programming_Language_(2nd_Edition)/Chapter_7#7.6)
4. Howard Hinnant's date library. [date.h](https://github.com/HowardHinnant/date)