---
title:                "Konwertowanie daty na ciąg znaków."
html_title:           "C: Konwertowanie daty na ciąg znaków."
simple_title:         "Konwertowanie daty na ciąg znaków."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest częstym i ważnym zadaniem w programowaniu. Dzięki temu można wyświetlić datę w czytelnej i zrozumiałej formie dla użytkownika, a także dokonywać porównań i obliczeń z jej wykorzystaniem.

## Jak to zrobić?

Aby przekonwertować datę na ciąg znaków w języku C, należy użyć funkcji `strftime()` z biblioteki `<time.h>`. Przyjmuje ona trzy argumenty: bufor, do którego zapisywany będzie wynik, maksymalny rozmiar bufora i format daty. Przykładowy kod wygląda następująco:

```C
#include <stdio.h>
#include <time.h>

int main() {
  char date_str[50];  // Bufor do zapisu wyniku
  time_t now = time(NULL);  // Pobranie aktualnej daty i czasu
  struct tm *time_info = localtime(&now);  // Konwersja do struktury tm

  // Ustawienie formatu daty (np. dzień-miesiąc-rok godzina:minuta)
  char *format = "%d-%m-%Y %H:%M"; 

  // Wywołanie funkcji strftime() z odpowiednimi argumentami
  strftime(date_str, sizeof(date_str), format, time_info); 

  // Wyświetlenie wyniku
  printf("Data i czas: %s\n", date_str); 

  return 0;
}
```

Możliwe formaty daty można znaleźć w dokumentacji funkcji `strftime()`. Pamiętajmy także, aby do konwersji używać struktur `time_t` i `struct tm` oraz uwzględnić odpowiednie nagłówki.

## Deep Dive

Bardziej zaawansowane operacje związane z konwersją daty mogą wymagać wykorzystania biblioteki `libdatetime`, która dostarcza funkcje do parsowania i formatowania daty i czasu w różnych językach i regionach. W przypadku pracy z wieloma strefami czasowymi można także skorzystać z funkcji `gmtime()` do zmiany daty i czasu na czas UTC.

Podczas konwersji daty należy także pamiętać o obsłudze ewentualnych błędów, takich jak nieprawidłowy format daty lub przepełnienie bufora.

## Zobacz też

- Dokumentacja funkcji `strftime()`: https://en.cppreference.com/w/c/chrono/strftime
- Dokumentacja biblioteki `libdatetime`: https://man7.org/linux/man-pages/man7/datetime.7.html
- Poradnik wyjaśniający różnice między struktuami `time_t` i `struct tm`: https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm