---
title:                "Сравнение двух дат"
date:                  2024-01-28T23:55:58.596799-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Сравнение двух дат заключается в выяснении их хронологического порядка — являются ли они одинаковыми, одна раньше или позже другой? Программисты делают это для сортировки событий, проверки временных промежутков и обработки бронирований. Это повседневное времяпрепровождение в коде.

## Как это сделать:

В C мы часто используем библиотеку `time.h` для работы с датами. Вот быстрый пример:

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // Преобразование в time_t для упрощения сравнения
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    // Сравнение
    if (t1 < t2) return -1; // date1 раньше
    if (t1 > t2) return 1;  // date1 позже
    return 0;               // даты одинаковы
}

int main() {
    // Две даты для сравнения
    struct tm date1 = { .tm_year = 120, .tm_mon = 5, .tm_mday = 14 }; // 2020-06-14
    struct tm date2 = { .tm_year = 122, .tm_mon = 11, .tm_mday = 3 };  // 2022-12-03

    int result = compare_dates(date1, date2);

    if (result < 0) {
        printf("Date1 раньше чем Date2.\n");
    } else if (result > 0) {
        printf("Date1 позже чем Date2.\n");
    } else {
        printf("Date1 такая же как Date2.\n");
    }

    return 0;
}
```

Пример вывода:
```
Date1 раньше чем Date2.
```

## Глубокое погружение

До того как `time.h` облагодетельствовала C стандартизированными функциями времени, вам приходилось создавать собственные сравнения дат — рискованное дело с високосными годами и всем таким. Теперь `mktime()` и `time_t` являются стандартным выбором. Они обрабатывают особенности календарей, так что вам этим не приходится заниматься.

`mktime()` принимает вашу дату в `struct tm` со всеми дружественными к человеку полями и превращает ее в значение `time_t`. Это значение представляет секунды с начала эпохи (00:00, 1 января 1970 года, UTC). Как только ваши даты преобразованы в `time_t`, это просто сравнение чисел.

Есть и более изысканные альтернативы, такие как `difftime()` для нахождения разницы во времени или использование сторонних библиотек. Они могут предложить больше функций, но для простого вопроса "Какая дата раньше?" стандартная библиотека обычно справится.

Реализация зависит от системных настроек времени — часовые пояса и летнее время могут ввести вас в заблуждение. `mktime()` интерпретирует `struct tm` как местное время, поэтому будьте внимательны при сравнении дат из разных часовых поясов.

## Смотрите также

- Справочник по C `time.h`: https://en.cppreference.com/w/c/chrono
- `time(7)` - обзор времени и даты в системах Unix: http://man7.org/linux/man-pages/man7/time.7.html
- Руководство по GNU C Library (glibc) о времени: https://www.gnu.org/software/libc/manual/html_node/Time.html