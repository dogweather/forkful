---
title:                "C++: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków może być bardzo użyteczna w programowaniu, ponieważ pozwala na wyświetlenie daty w czytelny sposób dla użytkownika, a także ułatwia porównywanie i manipulowanie datami wewnątrz programu.

## Jak to zrobić

Konwersja daty na ciąg znaków jest możliwa przy użyciu funkcji `std::to_string()` z biblioteki `<string>` oraz funkcji `std::to_string()` z biblioteki `<chrono>`. Poniżej znajdują się przykładowe kody w języku C++ oraz wynik ich działania:

```C++
#include <iostream>
#include <string>
#include <chrono>
using namespace std;

int main() {
    // przykładowa data
    chrono::system_clock::time_point date = chrono::system_clock::now();
    
    // konwersja daty na typ zmiennoprzecinkowy
    time_t time = chrono::system_clock::to_time_t(date);
    
    // konwersja daty na ciąg znaków
    string date_string = std::to_string(time);
    
    // wyświetlenie wyniku w konsoli
    cout << date_string << endl;
    
    return 0;
}
```

Wynik:

```
1621360375
```

## Głębsza analiza

Funkcja `std::to_string()` służy do konwertowania podstawowych typów danych na ciągi znaków, w tym także typu czasowego `time_t`. W przypadku konwersji daty na ciąg znaków, typ ten reprezentuje liczbę sekund, które upłynęły od początku epoki UNIX (1 stycznia 1970 roku). Aby uzyskać czytelny dla użytkownika wynik, należy skorzystać z funkcji z biblioteki `<chrono>`, która umożliwia dostęp do bardziej zaawansowanych operacji na czasie.

## Zobacz także

- [Dokumentacja biblioteki <string> w języku C++](https://en.cppreference.com/w/cpp/string)
- [Dokumentacja biblioteki <chrono> w języku C++](https://en.cppreference.com/w/cpp/chrono)