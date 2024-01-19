---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na string (lancuch znaków) w C++ to proces zamiany daty z formatu liczbowego na czytelny dla człowieka tekst. Programiści robią to, aby ułatwić odczytywanie dat i uatrakcyjnić interfejs użytkownika.

## Jak to zrobić:

Oto przykład w C++, jak można przekształcić datę na string:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    auto teraz = std::chrono::system_clock::now();
    std::time_t czas = std::chrono::system_clock::to_time_t(teraz);
    
    std::cout << "Teraz: " << std::put_time(std::localtime(&czas), "%Y-%m-%d %X") << std::endl;
    
    return 0;
}
```
Przykładowe wyjście:

```
Teraz: 2022-03-15 16:22:33
```
## Głębsze spojrzenie

(1) Kontekst historyczny: Przekształcanie dat na stringi jest praktyką starą jak samo programowanie. Format daty jest wyjątkowo istotny przy zapisie danych, aby były one spójne i zrozumiałe zarówno dla ludzi, jak i dla maszyn. (2) Alternatywy: Istnieje wiele bibliotek języka C++, takich jak Boost.Date_Time, które udostępniają narzędzia umożliwiające konwersję dat. (3) Szczegóły implementacji: W tym przypadku używamy biblioteki `<chrono>`, aby pobierać aktualny czas, a następnie konwertować go na format `std::time_t`, który następnie jest przekształcany na czytelny dla człowieka format za pomocą funkcji `std::put_time`.

## Zobacz także

1. Dokumentacja C++ - `<chrono>`: https://en.cppreference.com/w/cpp/chrono
2. Dokumentacja C++ - `put_time`: https://en.cppreference.com/w/cpp/io/manip/put_time
3. Boost.Date_Time: https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html
4. Przydatne narzędzia do konwersji dat: https://www.fluentcpp.com/2018/05/15/std-chronos-little-secret/