---
title:                "Porównywanie dwóch dat"
date:                  2024-01-20T17:32:35.319928-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Porównywanie dwóch dat to ustalenie, która jest wcześniejsza, późniejsza lub czy są identyczne. Programiści robią to, by zarządzać wydarzeniami, ważnością dokumentów czy okresami promocji.

## How to: (Jak to zrobić:)
```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm d1, struct tm d2) {
    if (d1.tm_year > d2.tm_year) return 1;
    if (d1.tm_year < d2.tm_year) return -1;
    if (d1.tm_mon > d2.tm_mon) return 1;
    if (d1.tm_mon < d2.tm_mon) return -1;
    if (d1.tm_mday > d2.tm_mday) return 1;
    if (d1.tm_mday < d2.tm_mday) return -1;
    return 0;
}

int main() {
    struct tm date1 = {.tm_year=123, .tm_mon=4, .tm_mday=5}; // Year is year-1900
    struct tm date2 = {.tm_year=123, .tm_mon=4, .tm_mday=6};

    int result = compare_dates(date1, date2);
    if(result > 0) {
        printf("Date1 is later than Date2\n");
    } else if(result < 0) {
        printf("Date1 is earlier than Date2\n");
    } else {
        printf("Date1 is the same as Date2\n");
    }

    return 0;
}
```
Output:
```
Date1 is earlier than Date2
```

## Deep Dive (Dogłębna analiza)
Porównywanie dat ma długą historię, zwłaszcza w aplikacjach bankowych i rezerwacyjnych. Alternatywą dla ręcznego porównywania jest użycie funkcji `difftime()` dla wartości `time_t` lub bibliotek zewnętrznych jak `date.h` w C++ lub `datetime` w Pythonie. Implementacja zależy od precyzji i zakresu dat, a także strefy czasowej, w której mają być interpretowane.

## See Also (Zobacz także)
- ISO 8601: Standard formatowania dat ([Wikipedia](https://en.wikipedia.org/wiki/ISO_8601))
- Funkcja difftime() w C ([cplusplus.com](http://www.cplusplus.com/reference/ctime/difftime/))
- Biblioteka `time.h` w C ([cplusplus.com](http://www.cplusplus.com/reference/ctime/))
- Książka "C Programming Language" Autorzy: Brian W. Kernighan, Dennis M. Ritchie
