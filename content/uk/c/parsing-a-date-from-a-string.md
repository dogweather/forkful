---
title:                "Розбір дати з рядка"
html_title:           "C: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?

Розбір дати з рядка - це процес витягування дати з рядкового значення. Це важлива задача для програмістів, оскільки часто у роботі з даними нам потрібно працювати з різними форматами дат, що може бути незручним. Розбір дати з рядка дозволяє програмістам ефективно опрацьовувати та зберігати дані.

Як:
```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    char date_string[] = "12/25/2021";
    char format[] = "%m/%d/%Y";
    struct tm tm;
    
    strptime(date_string, format, &tm);
    time_t t = mktime(&tm);
    
    printf("Parsed date: %s", ctime(&t));
    
    return 0;
}
```
Вивід:
```
Parsed date: Sat Dec 25 00:00:00 2021
```

Глибше:
Розбір дати з рядка має довгу історію. У минулому, коли використання дат у програмуванні не було настільки популярним, формати дат не були стандартизовані, що часто призводило до збоїв у роботі програм. Сьогодні існують різні методи розбору дат, включаючи використання бібліотек часу та парсерів дат.

Дивись також:
- https://www.programiz.com/c-programming/library-function/strptime
- https://www.geeksforgeeks.org/date-functions-in-c-c/
- https://www.epochconverter.com/programming/c