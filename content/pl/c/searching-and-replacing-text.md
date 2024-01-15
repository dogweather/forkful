---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "C: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Szukanie i zamiana tekstu są kluczowymi umiejętnościami w programowaniu. Pozwalają one na szybkie dostosowanie kodu do zmieniających się wymagań i ułatwiają jego czytelność. Ponadto, dzięki nim można automatycznie zmienić duże ilości tekstu, co znacznie przyspiesza pracę.

## Jak to zrobić

Mamy kilka sposobów na wyszukiwanie i zamianę tekstu w języku C. Najprostszym z nich jest użycie funkcji `strstr()` do znalezienia konkretnej frazy w tekście i funkcji `strncpy()` do jej zamiany. Przykładowy kod wyglądać może tak:

```C
char str[] = "Ten tekst zawiera słowo pomidor.";
char search[] = "pomidor";
char replace[] = "arbuz";
char *ptr;
ptr = strstr(str, search); // szukamy frazy "pomidor" w tekście
if (ptr != NULL) // jeśli udało się ją znaleźć
{
    strncpy(ptr, replace, strlen(replace)); // zamieniamy na "arbuz"
}
printf("%s", str); // nowy tekst: "Ten tekst zawiera słowo arbuz."
```

Jeśli mamy do zamiany wiele wystąpień danej frazy, możemy użyć pętli `while` do iteracji po tekście. Innym wygodnym sposobem jest użycie funkcji `str_replace()` dostępnej w bibliotece `string.h`. Poniżej przykładowy kod korzystający z tej funkcji:

```C
#include <string.h>
char str[] = "Ten tekst to tylko przykład.";
char search[] = "tylko";
char replace[] = "jedynie";
printf("%s", str_replace(str, search, replace)); // nowy tekst: "Ten tekst to jedynie przykład."
```

## Głębszy zanurzenie

W języku C jest też dostępnych wiele innych funkcji do manipulacji tekstem, takich jak `strtok()` do dzielenia tekstu na części, `strcat()` do łączenia tekstu czy `strlen()` do obliczania długości ciągu znaków. Ponadto, w przypadku bardziej skomplikowanych operacji można użyć wyrażeń regularnych za pomocą funkcji `regcomp()` i `regexec()`.

Podstawową zasadą przy wyszukiwaniu i zamianie tekstu jest pamiętanie o wielkości liter. Można to rozwiązać na przykład poprzez użycie funkcji `toupper()` lub `tolower()` do zmiany wszystkich liter na wielkie lub małe.

## Zobacz także

- Dokumentacja funkcji `string.h` w języku C: https://www.tutorialspoint.com/c_standard_library/string_h.htm
- Przewodnik po wyrażeniach regularnych w języku C: http://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html