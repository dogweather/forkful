---
title:                "Porównywanie dwóch dat"
aliases:
- /pl/c/comparing-two-dates.md
date:                  2024-02-03T17:53:37.521180-07:00
model:                 gpt-4-0125-preview
simple_title:         "Porównywanie dwóch dat"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat w języku C polega na ustaleniu, która z nich jest wcześniejsza, a która późniejsza, lub czy są takie same. Możliwość ta jest kluczowa w aplikacjach zajmujących się planowaniem, terminami czy prowadzeniem ewidencji, ponieważ pozwala na organizowanie i manipulowanie danymi wrażliwymi na czas.

## Jak to zrobić:

C nie posiada wbudowanego typu dla dat, co wymusza użycie biblioteki `time.h` do pracy ze strukturami daty i czasu. Struktura `tm` oraz funkcja `difftime()` są powszechnie używane do porównywania dat. Poniżej znajduje się przykład pokazujący, jak porównać dwie daty:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Pierwsza data (RRRR, MM, DD)
    date1.tm_year = 2023 - 1900; // Rok od 1900
    date1.tm_mon = 3 - 1;        // Miesiąc [0-11]
    date1.tm_mday = 15;          // Dzień miesiąca [1-31]

    // Druga data (RRRR, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Konwersja do formatu time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Porównanie
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("Daty są takie same.\n");
    } else if (seconds > 0) {
        printf("Pierwsza data jest późniejsza niż druga.\n");
    } else {
        printf("Pierwsza data jest wcześniejsza niż druga.\n");
    }

    return 0;
}
```

Wyjście może być takie:

```text
Pierwsza data jest wcześniejsza niż druga.
```

Ten program inicjuje dwie struktury `tm` określonymi datami, konwertuje je do formatu `time_t` za pomocą `mktime()` i finalnie porównuje je przy użyciu `difftime()`, który zwraca różnicę w sekundach (jako `double`) między dwoma czasami.

## Wnikliwa analiza

W początkowych dniach języka C, operacje na datach i czasie wymagały ręcznych obliczeń, często uwzględniających lata przestępne, zmienne liczby dni w miesiącach, a nawet sekundy przestępne. Wprowadzenie `time.h` do standardu ANSI C wprowadziło standaryzację obsługi czasu w C, upraszczając operacje na datach i czasie.

Użycie `time.h` do porównywania dat jest proste, ale ma swoje ograniczenia. Struktura `tm` nie uwzględnia stref czasowych ani czasu letniego, a `difftime()` dostarcza różnicę tylko w sekundach, brakując drobniejszego ziarnistości dla niektórych aplikacji.

Dla aplikacji wymagających bardziej zaawansowanych operacji na datach-czasie, w tym wsparcia dla stref czasowych, przejść na czas letni i dokładniejszych przedziałów czasowych, biblioteki takie jak `date.h` (biblioteka dat Howarda Hinnanta, niebędąca częścią standardowej biblioteki) oferują nowoczesną alternatywę dla `time.h`. Te biblioteki dostarczają bardziej kompleksowych narzędzi do manipulacji datą-czasem w C++, korzystając z dziesięcioleci ewolucji w projektowaniu języków programowania. Dla programistów C, wykorzystanie tych zewnętrznych bibliotek lub staranne radzenie sobie z zawiłościami obliczeń dotyczących daty-czasu bezpośrednio pozostaje konieczne do osiągnięcia precyzyjnej oraz kulturowo świadomej manipulacji datą-czasem.
