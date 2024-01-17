---
title:                "Pobieranie bieżącej daty"
html_title:           "C++: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?
Pobieranie aktualnej daty jest ważnym elementem w programowaniu, ponieważ często potrzebujemy aktualnych informacji w celu wykorzystania ich w naszym programie. Programiści używają tej funkcji do ustalania daty i godziny, rejestracji zdarzeń, sprawdzania ważności licencji i wielu innych zadań.

## Jak to zrobić:
Poniżej przedstawiamy przykładowy kod w języku ```C++```, który pomoże Ci uzyskać aktualną datę w formacie "dd/mm/yyyy":

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
  time_t now = time(0);
  char* dt = ctime(&now);
  cout << "Aktualna data i czas: " << dt << endl;
}
```

Output:

```
Aktualna data i czas: Thu Jun 17 18:05:36 2021
```

## Głębsza analiza:
Pobieranie aktualnej daty jest możliwe dzięki wykorzystaniu biblioteki ```ctime```, która dostarcza funkcje i struktury potrzebne do manipulowania czasem i datą w programie. Alternatywą dla tej metody jest użycie klasy ```std::chrono```, która oferuje większą precyzję i elastyczność przy operacjach na czasie.

## Zobacz także:
- Dokumentacja biblioteki ```ctime``` w języku C++: [https://en.cppreference.com/w/cpp/chrono/c/time](https://en.cppreference.com/w/cpp/chrono/c/time)
- Przykładowe zastosowania pobierania aktualnej daty w programowaniu: [https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- Porównanie metod pobierania aktualnej daty: [https://www.chrono-timer.com/cxx-time_fnc.html](https://www.chrono-timer.com/cxx-time_fnc.html)