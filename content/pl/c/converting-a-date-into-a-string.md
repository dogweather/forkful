---
title:                "Konwersja daty na łańcuch znaków"
date:                  2024-01-20T17:36:18.300113-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Konwersja daty na ciąg znaków pozwala wyświetlić i zapisać datę w czytelnej formie. Programiści robią to, by ukazać daty użytkownikom lub przygotować je do eksportu i dalszego przetwarzania.

## How to:
Jak to zrobić?
```c
#include <stdio.h>
#include <time.h>

int main() {
    // Pobierz aktualny czas
    time_t rawtime;
    time(&rawtime);

    // Konwertuj na lokalny czas
    struct tm *timeinfo = localtime(&rawtime);

    // Przygotuj ciąg znaków z datą
    char date_str[80];
    strftime(date_str, sizeof(date_str), "%Y-%m-%d %H:%M:%S", timeinfo);

    // Wyświetl datę
    printf("Aktualna data: %s\n", date_str);

    return 0;
}
```

Wyjście:
```
Aktualna data: 2023-04-05 14:45:31
```

## Deep Dive:
Głębsze spojrzenie: Funkcja `strftime()` pochodzi z C Standard Library i została wprowadzona w języku C w wersji C90. Alternatywnie można użyć `sprintf()` do prostszych formatów, ale `strftime()` jest bardziej elastyczna dla dat. Umożliwia niestandardowe formatowanie daty i czasu.
Podczas pracy na systemach Unixowych, czas UTC może być preferowany – wtedy `gmtime()` zastępuje `localtime()`. Implementacja tych funkcji używa struktury `tm`, która zawiera wszystkie składniki daty i czasu.

## See Also:
Zobacz również:
- Dokumentacja C Standard Library: https://en.cppreference.com/w/c/chrono
- Formatowanie daty i czasu w C: https://en.cppreference.com/w/c/chrono/strftime
- Porównanie funkcji czasu w C: https://en.cppreference.com/w/c/chrono
- Dyskusja o `strftime()` vs `sprintf()` dla dat: https://stackoverflow.com/questions/1442116/how-to-get-date-and-time-value-in-c-program
