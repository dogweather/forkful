---
title:    "C: Zdobycie aktualnej daty"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy projekt komputerowy wymaga często korzystania z bieżącej daty. Może to być potrzebne do śledzenia okresów czasowych, generowania raportów czy też do funkcji końcowych. W tym blogu przyjrzymy się jak w prosty sposób uzyskać aktualną datę w języku C.

## Jak to zrobić

W języku C istnieją różne sposoby na uzyskanie aktualnej daty. Najbardziej popularnym jest użycie funkcji `time()` oraz struktury `struct tm` z biblioteki `time.h`.

```C
#include <stdio.h>
#include <time.h>

int main() {
   // uzyskanie czasu w sekundach od początku epoki (01.01.1970 00:00:00)
   time_t now;

   // użycie funkcji time()
   time(&now);

   // konwersja do struktury tm
   struct tm* local = localtime(&now);

   // wyświetlenie bieżącej daty
   printf("Aktualna data: %d.%d.%d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);

   return 0;
}
```

**Output:**

```
Aktualna data: 17.08.2021
```

Inną metodą jest użycie funkcji `ctime()`, która zwraca bieżącą datę w formie tekstu.

```C
#include <stdio.h>
#include <time.h>

int main() {
   // uzyskanie czasu w sekundach od początku epoki (01.01.1970 00:00:00)
   time_t now;

   // użycie funkcji time()
   time(&now);

   // wyświetlenie bieżącej daty
   printf("Aktualna data: %s\n", ctime(&now));

   return 0;
}
```

**Output:**

```
Aktualna data: Tue Aug 17 12:01:27 2021
```

Można również wykorzystać funkcję `strftime()` w celu dostosowania formatu wyświetlanej daty.

```C
#include <stdio.h>
#include <time.h>

int main() {
   // uzyskanie czasu w sekundach od początku epoki (01.01.1970 00:00:00)
   time_t now;

   // użycie funkcji time()
   time(&now);

   // konwersja do struktury tm
   struct tm* local = localtime(&now);

   // deklaracja bufora
   char buffer[50];

   // użycie funkcji strftime()
   strftime(buffer, 50, "%d/%m/%Y", local);

   // wyświetlenie bieżącej daty
   printf("Aktualna data: %s\n", buffer);

   return 0;
}
```

**Output:**

```
Aktualna data: 17/08/2021
```

## Deep Dive

Bieżąca data jest przechowywana w języku C jako liczba całkowita reprezentująca liczbę sekund od początku epoki. Jest to 32-bitowa liczba, co oznacza, że może przechowywać daty do 3 stycznia 2038 roku. Jednym ze sposobów na rozszerzenie tego zakresu jest użycie 64-bitowej liczby z biblioteki `inttypes.h`.

Innym ciekawym elementem jest fakt, że bieżąca data jest zależna od strefy czasowej. Funkcja `localtime()` konwertuje liczbę sekund na lokalną strefę czasową, dlatego może zwrócić inną wartość dla różnych miejsc na świecie.

## Zobacz również

- [Dokumentacja funkcji time() w języku C (en)](https://en.cppreference.com/w/c/chrono/time)
- [Informacje o strukturze struct tm (en)](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Porównanie różnych sposobów uzyskania bieżącej daty w ję