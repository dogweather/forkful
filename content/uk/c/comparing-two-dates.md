---
title:    "C: Порівняння двох дат"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні час є невід’ємною частиною роботи. Порівняння двох дат є важливим аспектом розробки програм, особливо для тих, що пов’язані зі збереженням та обробкою даних.

## Як

Для порівняння двох дат у мові програмування C використовується функція `difftime()`. Ось приклад коду та його виведення:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // створення структури часу для першої дати
    struct tm date1 = { .tm_year = 2020, .tm_mon = 6, .tm_mday = 1 };
    // створення структури часу для другої дати
    struct tm date2 = { .tm_year = 2021, .tm_mon = 6, .tm_mday = 1 };

    // обчислення різниці між датами
    double diff = difftime(mktime(&date2), mktime(&date1));

    // виведення результату
    printf("Різниця між датами у секундах: %f\n", diff);

    return 0;
}
```

Вивід:

```
Різниця між датами у секундах: 31536000.000000
```

## Глибокий занурення

У мові програмування C дати представлені структурою `struct tm`, яка містить різні значення, такі як рік, місяць, день, година тощо. Функція `difftime()` повертає різницю у секундах між двома датами, яку можна використовувати, наприклад, для обчислення кількості днів, годин або хвилин між датами.

## Дивіться також

- [How to Compare Dates in C Programming](https://www.programiz.com/c-programming/examples/difference-dates)
- [C Date and Time](https://www.cprogramming.com/tutorial/c/lesson15.html)
- [C Standard Library - <time.h>](https://en.cppreference.com/w/c/chrono/difftime)