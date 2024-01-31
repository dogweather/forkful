---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:28:42.892523-07:00
model:                 gpt-4-1106-preview
html_title:           "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Po co obliczać daty przyszłe lub przeszłe? Otóż, by ustalić terminy wydarzeń, okresy ważności, harmonogramy. Programiści robią to, by aplikacje mogły inteligentnie zarządzać czasem.

## How to:
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;

    time(&rawtime);
    timeinfo = localtime(&rawtime);
    printf("Aktualna data i czas: %s", asctime(timeinfo));

    // Dodanie 7 dni do aktualnej daty
    timeinfo->tm_mday += 7;
    mktime(timeinfo);
    printf("Data za tydzień: %s", asctime(timeinfo));

    // Odejmowanie 30 dni od aktualnej daty
    timeinfo->tm_mday -= 30;
    mktime(timeinfo);
    printf("Data 30 dni temu: %s", asctime(timeinfo));

    return 0;
}
```
## Deep Dive:
W C używamy `time.h` do operacji na datach. Funkcje takie jak `time`, `localtime`, czy `mktime` pojawiają się od lat 70. jako część standardu ANSI C.

Alternatywy: możesz wykorzystać biblioteki zewnętrzne jak `date.h` od Howarda Hinnanta dla większej precyzji i funkcjonalności.

Detale implementacyjne: Pamiętaj, że funkcja `mktime` normalizuje strukturę `tm`. Oznacza to, że przeliczy godziny na dni, dni na miesiące itd., jeśli dodasz np. 25 godzin.

## See Also:
- [cplusplus.com - `<ctime>` (time.h)](http://www.cplusplus.com/reference/ctime/)
- [Howard Hinnant's Date Library](https://github.com/HowardHinnant/date)
