---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:26.921523-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? / Co i Dlaczego?
Pobieranie aktualnej daty polega na zapisaniu informacji o obecnym dniu, miesiącu i roku w programie. Programiści robią to, aby rejestrować zdarzenia, określać terminy, czy personalizować interakcje z użytkownikami.

## How to: / Jak to zrobić:
Poniżej znajdziesz przykładowy kod w C, który pokazuje, jak pobrać i wyświetlić aktualną datę:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    struct tm * time_info;
    char timeString[9]; // YYYY-MM-DD

    time(&current_time);
    time_info = localtime(&current_time);

    strftime(timeString, sizeof(timeString), "%Y-%m-%d", time_info);
    printf("Current Date: %s\n", timeString);

    return 0;
}
```

Wyjście przykładowe:
```
Current Date: 2023-04-12
```

## Deep Dive / Dogłębna analiza:
Funkcja `time()` zwraca aktualny czas jako typ `time_t`, który reprezentuje liczbę sekund od północy (UTC) 1 stycznia 1970 roku, znanej jako Unix Epoch. Używamy `localtime()` do konwersji `time_t` na bardziej czytelną strukturę `tm`. Do sformatowania daty służy funkcja `strftime()`.

Alternatywy? Możesz użyć `gettimeofday()` jeżeli potrzebujesz większej precyzji (mikrosekundy) albo `clock()` do mierzenia czasu CPU wykorzystanego przez program.

Ważne jest, by pamiętać o standaryzowaniu obsługi czasu w aplikacjach wieloplatformowych. Różne systemy mogą inaczej interpretować i przechowywać wartości typu `time_t`.

## See Also / Zobacz również:
- Dokumentacja online funkcji C do obsługi czasu: https://en.cppreference.com/w/c/chrono
- Tutorial na temat `struct tm` i związanych funkcji: http://www.cplusplus.com/reference/ctime/tm/
- Wyjaśnienie Unix Epoch: https://en.wikipedia.org/wiki/Unix_time