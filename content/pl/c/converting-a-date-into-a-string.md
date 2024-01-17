---
title:                "Konwersja daty na ciąg znaków"
html_title:           "C: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie daty na łańcuch znaków to proces, w którym datę lub czas wyraża się w postaci tekstu zamiast w jej oryginalnej postaci numerycznej. Programiści często wykonują tę czynność, ponieważ ułatwia to wyświetlanie daty w czytelnej i łatwej do zrozumienia formie dla użytkowników aplikacji.

## Jak to zrobić:

```C
// Przykładowy program konwertujący datę na łańcuch znaków
#include <stdio.h>
#include <time.h>

int main() {
  // Deklaracja i inicjalizacja zmiennej typu time_t
  time_t seconds = time(NULL);
  
  // Użycie funkcji localtime() do pobrania daty i czasu
  // w postaci struktury tm
  struct tm *now = localtime(&seconds);
  
  // Użycie funkcji strftime() do konwersji struktury tm
  // na łańcuch znaków
  char buffer[80];
  strftime(buffer, 80, "Dzisiaj jest %d.%m.%Y, a godzina to %H:%M:%S", now);
  
  // Wyświetlenie łańcucha znaków
  printf("%s", buffer);
  
  return 0;
}
```

Przykładowe wyjście:

```
Dzisiaj jest 22.07.2021, a godzina to 16:30:00
```

## Głębszy zanurzenie:

Konwersja daty na łańcuch znaków jest powszechnie stosowanym procesem w programowaniu. Początkowo, programiści musieli ręcznie formatować datę w łańcuch znaków, używając odpowiednich sekwencji formatujących. Jednak od czasu wydania standardu języka C w 1999 roku, dostępna jest funkcja strftime(), która znacznie ułatwiła ten proces.

Alternatywne metody konwersji daty na łańcuch znaków obejmują użycie funkcji sprintf() lub biblioteki zewnętrznej. Jednak funkcja strftime() jest często preferowanym wyborem ze względu na swoją prostotę i dostępność w standardzie języka C.

## Zobacz także:

- [Dokumentacja funkcji strftime() w języku C](https://www.cplusplus.com/reference/ctime/strftime/)
- [Porównanie różnych metod konwersji daty na łańcuch znaków w języku C](https://www.geeksforgeeks.org/converting-strings-numbers-cc-stdlib-vs-stringstream/)