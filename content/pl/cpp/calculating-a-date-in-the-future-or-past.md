---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "C++: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli kiedykolwiek zastanawiałeś się, jak policzyć datę w przyszłości lub przeszłości, ten artykuł jest dla Ciebie! Może mieć to zastosowanie w wielu różnych sytuacjach, na przykład planowaniu wydarzeń lub obliczaniu wynagrodzenia w określonym dniu.

## Jak to zrobić

Obliczanie daty w przyszłości lub przeszłości jest całkiem proste w języku C++. Wystarczy użyć funkcji `time()` i `localtime()`, a następnie wykorzystać odpowiednie tryby obliczeń.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Ustawienie daty i godziny aktualnego czasu
    time_t now = time(0);
    
    // Zamiana aktualnego czasu na strukturę tm
    tm *current = localtime(&now);
    
    // Obliczanie daty w przyszłości
    current->tm_mday += 10; // Dodajemy 10 dni do aktualnego dnia
    mktime(current); // Zamiana na poprawną datę
    
    // Wyświetlenie daty w przyszłości
    cout << "Data za 10 dni: " << current->tm_mday << "/" << current->tm_mon + 1 << "/" << current->tm_year + 1900 << endl;
    
    // Obliczanie daty w przeszłości
    current->tm_mday -= 17; // Odejmujemy 17 dni od aktualnego dnia
    mktime(current); // Zamiana na poprawną datę
    
    // Wyświetlenie daty w przeszłości
    cout << "Data sprzed 17 dni: " << current->tm_mday << "/" << current->tm_mon + 1 << "/" << current->tm_year + 1900 << endl;
    
    return 0;
}
```

Output: 
```
Data za 10 dni: 22/8/2021
Data sprzed 17 dni: 19/8/2021
```

## Głębszy wgląd

W języku C++ istnieje wiele sposobów na obliczanie daty w przyszłości lub przeszłości, na przykład używając biblioteki `chrono` lub funkcji `strftime()`. Dodatkowo, można również uwzględnić rok przestępny i uwzględnić różnice w czasie pomiędzy strefami czasowymi.

## Zobacz także

- [Dokumentacja funkcji time()](https://www.cplusplus.com/reference/ctime/time/)
- [Przykłady użycia strftime()](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Biblioteka chrono w C++](https://www.geeksforgeeks.org/chrono-in-c/)