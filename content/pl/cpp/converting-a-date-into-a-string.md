---
title:    "C++: Konwersja daty na ciąg znaków"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na łańcuch znaków jest ważnym elementem programowania w C++. Pozwala nam wyświetlać daty w czytelny sposób, dostosowany do preferencji użytkowników.

## Jak to zrobić

Konwertowanie daty na łańcuch znaków w języku C++ jest bardzo proste. Wystarczy użyć funkcji `std::to_string()`, która zamienia liczbę na łańcuch znaków. Poniżej znajduje się przykładowy kod:

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main()
{
    // Pobranie aktualnej daty
    time_t now = time(0);
    
    // Przekonwertowanie daty na łańcuch znaków
    string str_date = std::to_string(now);
    
    // Wyświetlenie daty w formacie MM/DD/RRRR
    cout << str_date.substr(4,2) << "/"
         << str_date.substr(6,2) << "/"
         << str_date.substr(0,4) << endl;
    
    return 0;
}
```

Przykładowy output: `10/31/2021`

## Głębszy wgląd

Konwertowanie daty na łańcuch znaków jest często wykorzystywane w celu wyświetlenia daty w czytelnej formie lub zapisania jej w pliku. Funkcja `std::to_string()` jest szczególnie przydatna, ponieważ pozwala na konwersję daty w różnych formatach (np. rok-miesiąc-dzień) bez dodatkowego formatowania.

Ważnym elementem konwersji jest również użycie odpowiedniego typu zmiennej. W przypadku daty, powinniśmy użyć typu `time_t`, który przechowuje liczbę sekund od 1 stycznia 1970. W ten sposób możemy łatwo przekonwertować datę na łańcuch znaków i wyświetlić ją w formacie, który najbardziej odpowiada naszym potrzebom.

## Zobacz także

- [Dokumentacja funkcji std::to_string()](https://en.cppreference.com/w/cpp/string/basic_string/to_string)
- [Inne przydatne funkcje związane z datami w C++](https://www.geeksforgeeks.org/c-datetime-functions-time-header/)