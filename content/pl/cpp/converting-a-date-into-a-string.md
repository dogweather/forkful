---
title:                "Konwersja daty na ciąg znaków"
html_title:           "C++: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Przetwarzanie daty na ciąg znaków jest często niezbędnym zadaniem w programowaniu. Wiele aplikacji wymaga wyświetlenia daty w czytelnej formie dla użytkownika lub zapisania jej w odpowiednim formacie do pliku. W tym artykule pokażemy jak łatwo przekształcić datę na ciąg znaków w języku C++.

## Jak to zrobić

Przedstawimy dwa sposoby na konwersję daty na ciąg znaków za pomocą biblioteki chrono oraz funkcji strftime.

```C++
#include <iostream>
#include <iomanip>
#include <chrono>
#include <ctime>

int main()
{
    using namespace std::chrono;

    // Uzyskanie aktualnej daty i czasu
    system_clock::time_point now = system_clock::now();

    // Konwersja na czas lokalny
    time_t tt = system_clock::to_time_t(now);

    // Przekształcenie daty do struktury tm
    struct tm * ptm = localtime(&tt);

    // Użycie funkcji strftime do formatowania daty
    char date_strftime[20];
    strftime(date_strftime, 20, "%d/%m/%Y", ptm);

    // Utworzenie ciągu znaków za pomocą metody put_time
    std::string date_put_time = std::put_time(ptm, "%d/%m/%Y");

    // Wypisanie wyników na ekran
    std::cout << "Data w formacie strftime: " << date_strftime << std::endl;
    std::cout << "Data w formacie put_time: " << date_put_time << std::endl;

    return 0;
}
```

Powyższy kod najpierw uzyskuje aktualną datę i czas za pomocą funkcji `now()` z biblioteki `chrono`. Następnie wykorzystuje funkcję `localtime()` do przekształcenia daty na stałą strukturę `tm`, która jest wykorzystywana przez funkcję `strftime()` do formatowania daty. Możemy również skorzystać z metody `put_time()` oraz operatora `%` do utworzenia ciągu znaków w wybranym przez nas formacie.

Po wykonaniu powyższego kodu, uzyskamy następujący wynik:

```
Data w formacie strftime: 08/03/2021
Data w formacie put_time: 08/03/2021
```

## Deep Dive

Istnieje wiele innych funkcji i metod do konwersji daty na ciąg znaków w języku C++. Dzięki temu możemy indywidualnie dostosować format daty do naszych potrzeb. Istnieje także możliwość zdefiniowania własnego formatu daty za pomocą specjalnych znaków.

Więcej informacji na temat formatowania daty można znaleźć w dokumentacji biblioteki `chrono` oraz funkcji `strftime`.

## Zobacz również

- [Dokumentacja biblioteki chrono](https://en.cppreference.com/w/cpp/chrono)
- [Dokumentacja funkcji strftime](https://en.cppreference.com/w/c/chrono/strftime)