---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Uzyskiwanie aktualnej daty to operacja, która pozwala na odczytywanie bieżącej daty i czasu z systemu. Programiści robią to, aby logować zdarzenia, ustalać terminy wygaśnięcia i śledzić czas w aplikacjach.

## Jak to zrobić:

Poniżej znajduje się prosty kod w C, który pokazuje, jak uzyskać aktualną datę i czas.

```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t aktualny_czas;
    struct tm * struktura_czasu;

    time(&aktualny_czas);
    struktura_czasu = localtime(&aktualny_czas);

    printf("Aktualna data i czas: %s", asctime(struktura_czasu));
    return 0;
}
```

Po skompilowaniu i uruchomieniu powyższego kodu, wyjście powinno wyglądać mniej więcej tak:
```
Aktualna data i czas: Sat Jan 15 11:52:20 2022
```

## Deep Dive

Pobieranie aktualnej daty i czasu jest funkcją programowania, która pozostała stosunkowo nienaruszona od początków języka C. Pomimo że istnieją biblioteki, takie jak `Boost` i `Poco`, które oferują bardziej rozbudowane funkcje do manipulacji datą i czasem, standardowa biblioteka `time.h` nadal jest najczęściej używaną opcją ze względu na jej prostotę i wszechstronność.

Alternatywą dla `localtime()` jest funkcja `gmtime()`, która zwraca czas UTC zamiast czasu lokalnego. Wybór między tymi dwoma zależy od konkretnej aplikacji i wymagań programistej.

Co ciekawe, `time()` zwraca czas w formacie `time_t`. Jest to typ danych w języku C reprezentujący liczbę sekund od północy 1 stycznia 1970 roku UTC, określany jako "czas epoki". Następnie `localtime()` przekształca `time_t` z powrotem na bardziej użyteczny format do wyświetlania.

## Zobacz też

1. Dokumentacja ISO C: https://www.iso.org/standard/74528.html
2. Dokumentacja Biblioteki C GNU: https://www.gnu.org/software/libc/manual/
3. Alternatywy dla `time.h` - Biblioteka Boost (https://www.boost.org/) i Biblioteka Poco (https://pocoproject.org/).