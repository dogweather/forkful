---
title:                "C: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest ważną umiejętnością w programowaniu, gdyż pozwala ona na dokładne sprawdzenie, który dzień jest wcześniejszy lub późniejszy. Może to być przydatne w wielu przypadkach, na przykład w systemach rezerwacji czy aplikacjach bankowych.

## Jak to zrobić

Aby porównać dwie daty w języku C, najpierw musimy zadeklarować zmienne typu `struct tm`, które będą reprezentować każdą z dat. Następnie używamy funkcji `mktime()`, która przetwarza datę i czas w sekundy od 1 stycznia 1970 roku. Dzięki temu możemy łatwo porównać obie daty za pomocą zwykłego operatora porównania `>`, `<` lub `==`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = { .tm_year = 121, .tm_mon = 4, .tm_mday = 1 }; // 1 maja 2021
    time_t seconds1 = mktime(&date1);

    struct tm date2 = { .tm_year = 120, .tm_mon = 7, .tm_mday = 21 }; // 21 sierpnia 2020
    time_t seconds2 = mktime(&date2);

    if (seconds1 > seconds2) {
        printf("Pierwsza data jest późniejsza.");
    }
    else if (seconds1 < seconds2) {
        printf("Druga data jest późniejsza.");
    }
    else {
        printf("Obie daty są takie same.");
    }
    
    return 0;
}
```

Output:

```
Pierwsza data jest późniejsza.
```

## Głębsze zagadnienia

W języku C obiekt `struct tm` przechowuje datę i czas w postaci składowych, takich jak rok, miesiąc, dzień itp. Dzięki temu możemy dokładnie porównać, który z tych elementów jest wcześniejszy lub późniejszy w obu datach.

Innym sposobem porównywania dat jest konwersja ich na format liczbowy, np. rok 2021 będzie reprezentowany przez liczbę 2021. W ten sposób możemy także dokładniej ustalić, którą datę później wydarzyło się wydarzenie.

## Zobacz również

- Dokumentacja języka C: https://linux.die.net/man/3/mktime
- Tutorial o porównywaniu dat w C: https://www.programminginmath.com/c-programming-compare-two-date/

Dzięki tej wiedzy możemy sprawnie porównywać daty w języku C i wykorzystywać to w różnego rodzaju aplikacjach. Miejmy na uwadze, że dokładność porównywania dat może różnić się w zależności od systemu czy ustawień regionalnych. Dlatego też warto skonsultować się z dokumentacją i testować nasze rozwiązania na różnych środowiskach. Powodzenia w programowaniu!