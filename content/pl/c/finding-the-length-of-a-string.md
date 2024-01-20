---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Długość łańcucha to ilość znaków zawartych w łańcuchu. Programiści muszą znać długość łańcucha do poprawnego manipulowania danymi i do złudzenia precyzyjnego zarządzania pamięcią.

## Jak to zrobić:

C provide `strlen()` do znalezienia długości łańcucha. Przyjrzyjmy się przykładowemu kodowi:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str[100];
   int len;

   printf("Wprowadź łańcuch: ");
   gets(str);

   len = strlen(str);
   printf("Długość łańcucha to: %d", len);

   return 0;
}
```
Na przykład, gdy wprowadzisz "Hello, World!", Output to "Długość łańcucha to: 13".

## Pogłębiona analiza

Pomimo że `strlen()` jest łatwy do użycia i często stosowany, ma również swoje wady. Funkcja biega przez łańcuch do momentu natrafienia na null-terminator ('\0'), co sprawia, że nie jest to najwydajniejsza metoda.

Alternatywą dla `strlen()` jest iterowanie przez łańcuch samodzielnie używając pętli, co daje więcej kontroli nad procesem. Pamiętaj, że każdy łańcuch zakończy się null-terminatorem.

## Zobacz również
