---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і чому це важливо?
Парсинг дати з рядка - це процес перетворення текстової рівніці, що представляє багато числових шматків, в практично значущі дати або часові етапи. Програмісти роблять це, щоб правильно розподілити та використати ці шматки в контексті дати чи часу.

## Як це зробити:
```C 
#include <time.h>
#include <stdio.h>

int main() {
    struct tm tm;
    char buf[255];

    printf("Enter a date (dd-mm-yyyy): ");
    fgets(buf,255,stdin);
    strptime(buf, "%d-%m-%Y", &tm);
    printf("Year: %d; Month: %d; Day: %d; \n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
}
```
У вище поданому коді, ми записуємо стрічку рівнянь, перетворюємо її в числові значення дати і виводимо на екран.

## Занурення у глибини
Парсинг дати з рядка був складним завданням для програмістів протягом декількох декад. Ще у 70-х роках 20-го століття, коли С було вперше створено, програмісти повинні були писати власні бібліотеки для вирішення цієї проблеми. 

Функція sscanf з стандартної бібліотеки С, що широко використовувалась у минулому, тепер вважається менш ефективною для парсингу дат. Альтернатива, 'strptime', у міру часу довела свою ефективність і тепер вважається надійнішим варіантом.

## Більше інформації
1. [Man Page для strptime](https://man7.org/linux/man-pages/man3/strptime.3.html)
2. [C Library Functions - Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/)
3. [Linux Time - Tutorialspoint](https://www.tutorialspoint.com/unix_time/index.htm)
4. [С Unix Time на Wikipedia](https://uk.wikipedia.org/wiki/Unix-%D1%87%D0%B0%D1%81)