---
title:                "Pobieranie aktualnej daty"
html_title:           "C: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie aktualnej daty jest procesem polegającym na uzyskaniu informacji o bieżącym dniu, miesiącu i roku. Programiści często wykonują tę czynność, aby móc śledzić czas wykonywania swoich programów lub aby ustawić datę w swoich aplikacjach.

## Jak to zrobić:
```C
#include <stdio.h>
#include <time.h>

int main(void) {
  struct tm *info;
  char buffer[80];

  time_t current_time = time(NULL);
  info = localtime(&current_time);
  strftime(buffer, 80, "Dzisiaj jest %d.%m.%Y", info);
  printf("%s\n", buffer);
  return 0;
}
```
W tym przykładzie kodu wykorzystujemy bibliotekę `<time.h>` aby uzyskać dostęp do aktualnej daty. Używając funkcji `localtime()` i `strftime()`, przetwarzamy informacje o dacie na żądany przez nas format, tj. "Dzisiaj jest DD.MM.RRRR" i wyświetlamy ją za pomocą funkcji `printf()`.

## Głębsze zagadnienia:
Pobieranie aktualnej daty jest możliwe dzięki zastosowaniu standardu C czasu poszukiwania. Alternatywnym podejściem jest wykorzystanie funkcji `time()` lub `clock()` dla pomiarów czasu wykonywania programu.

## Zajrzyj także:
Więcej informacji na temat pobierania aktualnej daty w języku C można znaleźć w dokumentacji biblioteki `<time.h>` oraz w poniższych źródłach:

- [Funkcja time() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Pobieranie aktualnego czasu w C](https://www.geeksforgeeks.org/c-program-display-current-date-time/)