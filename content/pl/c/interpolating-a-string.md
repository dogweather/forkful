---
title:                "Interpolacja ciągu znaków"
html_title:           "C: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja ciągów znaków to proces łączenia zmiennych z danymi tekstowymi w celu uzyskania końcowego ciągu znaków. Programiści używają jej, aby dynamicznie tworzyć ciągi znaków, których nie można z góry przewidzieć.

## Jak to zrobić:

```C
#include <stdio.h>

int main() {
   char imie[] = "Anna";
   int wiek = 23;
   char miasto[] = "Warszawa";
   char zyczenia[] = "Witaj %s, dzięki że jesteś z nami już %d lat w %s!";
   printf(zyczenia, imie, wiek, miasto);

   return 0;
}
```
Wynik:
```
Witaj Anna, dzięki że jesteś z nami już 23 lata w Warszawie!
```

## Głębszy zanurzenie:

Interpolacja ciągów znaków jest popularną techniką programistyczną, która ma swoje korzenie jeszcze w języku C. Alternatywą dla niej może być łączenie ciągów znaków za pomocą funkcji strcat(), jednakże interpolacja jest uważana za bardziej czytelną i wygodniejszą. W C, do interpolacji używa się funkcji sprintf(), która działa podobnie do funkcji printf(). W celu uniknięcia błędów, ważne jest, aby upewnić się, że argumenty użyte w interpolacji są zgodne z formatem ciągu znaków.

## Zobacz też:

- Dokumentacja języka C: https://www.cs.cf.ac.uk/Dave/C/node6.html
- Inne metody łączenia ciągów znaków: https://www.youtube.com/watch?v=3DMlyp6zEh0
- Przykładowe ćwiczenia z interpolacji w C: http://www.lukasz-socha.pl/kurs-c/c-05-03-interpolacja-ciagow-znakow