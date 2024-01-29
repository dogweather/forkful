---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:55:50.455296-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Расчет будущей или прошлой даты подразумевает определение точного дня, который находится на конкретном интервале от известной даты. Программисты делают это для планирования событий, окончания действия токенов, напоминаний и т. д.

## Как это сделать:

Вот прямолинейный код на C для расчета даты в будущем. Мы используем функции из `time.h`.

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm new_date;
    double daysToAdd = 10; // 10 дней в будущее

    // Получить текущее время и преобразовать в структуру tm
    time(&now);
    new_date = *localtime(&now);

    // Добавляем дни к текущей дате
    new_date.tm_mday += daysToAdd;
    mktime(&new_date);

    // Вывод новой даты:
    printf("Дата через 10 дней будет: %02d-%02d-%04d\n",
           new_date.tm_mday,
           new_date.tm_mon + 1, // tm_mon 0-11
           new_date.tm_year + 1900); // tm_year это годы с 1900

    return 0;
}
```

Пример вывода: `Дата через 10 дней будет: 12-04-2023`

## Подробнее

В прошлом расчет будущих или прошлых дат был сложной задачей - никаких встроенных функций, только чистое алгоритмическое удовольствие. Теперь `time.h` в C предоставляет вам `time_t`, `struct tm` и функции, такие как `mktime()`, чтобы упростить жизнь.

Альтернативы? Конечно, есть. Для сложной манипуляции с датой и временем некоторые разработчики используют библиотеки типа `date.h` для C++ или модуль 'chrono'.

Детали? Функция `mktime()` нормализует `struct tm`. Это означает, что если вы добавите 40 к дням, это перейдет на месяцы, даже годы. Полезно знать, чтобы не изобретать свою собственную машину времени, ходящую по кругу.

## Смотрите также

- Cтандартная библиотека C - `time.h`: https://en.cppreference.com/w/c/chrono
- Альтернативные библиотеки для работы с датой и временем, например, библиотека `date.h` от Howard Hinnant для C++: https://github.com/HowardHinnant/date
- Объяснения функции `mktime()`: https://www.cplusplus.com/reference/ctime/mktime/
