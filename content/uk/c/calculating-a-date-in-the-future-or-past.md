---
title:                "Обчислення дати у майбутньому або минулому."
html_title:           "C: Обчислення дати у майбутньому або минулому."
simple_title:         "Обчислення дати у майбутньому або минулому."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Причини

Розрахунок дати в майбутньому або минулому може бути потрібним для планування подій, визначення строків або навіть просто для цікавості. Наприклад, можна обчислити, коли закінчиться термін дії страхового поліса або яке буде день народження за 10 років.

## Як це зробити

Для розрахунку дати використовується функція `mktime` з бібліотеки `time.h`. Приклад коду для обчислення дати через 10 років виводиться на екран:

```C
#include <stdio.h>
#include <time.h>

int main() 
{
    // Отримуємо поточну дату та час
    time_t current_time = time(NULL);

    // Обчислюємо дату через 10 років від поточної
    time_t future_time = current_time + (10 * 365 * 24 * 60 * 60);

    // Конвертуємо час у локальний час
    struct tm *local_time = localtime(&future_time);

    // Виводимо обчислену дату
    printf("Дата через 10 років: %d/%d/%d", local_time->tm_mon + 1, local_time->tm_mday, local_time->tm_year + 1900);

    return 0;
}
```

Вивід на екран буде таким:

```
Дата через 10 років: 10/1/2031
```

## Поглиблене дослідження

Функція `mktime` приймає структуру `tm` з даними про дату та час і повертає кількість секунд, які пройшли з 1 січня 1970 року (відомий як "UNIX час"). Для розрахунку майбутніх чи минулих дат потрібно лише додати або відняти потрібну кількість секунд від поточного часу і конвертувати результат у бажаний формат виводу.

## Дивись також

- [Розрахунок дати в майбутньому або минулому за допомогою бібліотеки `date.h`](https://www.geeksforgeeks.org/finding-date-after-given-number-days-in-c/)
- [Довідник по бібліотеці `time.h`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Програмування на мові C: вступ у світ коду](https://www.freecodecamp.org/news/an-introduction-to-c-programming/)