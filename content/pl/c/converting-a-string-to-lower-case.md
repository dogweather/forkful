---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "C: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Konwersja tekstu na małe litery jest procesem zmiany wszystkich liter w ciągu znaków na ich niskie odpowiedniki. Programiści często wykonują tę operację, ponieważ zapewnia to jednolitą formę danych, ułatwiając porównywanie i przetwarzanie tekstu.

## Jak to zrobić:

``` C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int main() {
   char str[] = "PRZYKLADOWY TEKST";
   int i = 0;

   while(str[i]) {
      putchar(tolower(str[i]));
      i++;
   }

   return 0;
}
```

Output:
```
przykladowy tekst
```

## Głębsza analiza:

1. Kontekst historyczny:
Konwersja tekstu na małe litery ma swoje korzenie w dawnych systemach komputerowych, które używały tylko dużych liter. Konieczność znormalizowania danych doprowadziła do rozwoju funkcji konwersji.

2. Alternatywy:
Poza funkcją `tolower()`, istnieje również funkcja `strlwr()`, która wykonuje tę samą operację, ale dla całego ciągu znaków. Można także użyć pętli `for` lub funkcji `tolower()` z biblioteki `string.h`.

3. Szczegóły implementacji:
W C, funkcja `tolower()` jest częścią biblioteki `ctype.h`, która dostarcza zestaw funkcji do analizy i manipulacji danych tekstowych. Funkcja ta wykorzystuje kod ASCII każdego znaku, aby określić jego niski odpowiednik.

## Zobacz także:

- [Funkcja `tolower()` w C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Inne funkcje z biblioteki `ctype.h`](https://www.geeksforgeeks.org/ctype-h-header-file-c/)