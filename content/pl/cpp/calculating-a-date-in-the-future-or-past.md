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

## Co & Dlaczego?

Obliczanie daty w przeszłości lub przyszłości jest częstym zadaniem programistów, która polega na wyznaczaniu daty, która jest określoną ilość dni w przyszłości lub przeszłości od danej daty. Programiści często wykonują to zadanie w swoich projektach, ponieważ pozwala to na śledzenie i prognozowanie przyszłych wydarzeń.

## Jak?

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // obecna data
    time_t t = time(NULL);
    tm* now = localtime(&t);

    // obliczenie daty w przeszłości - 30 dni temu
    now->tm_mday -= 30;
    mktime(now);

    // wyświetlenie daty
    cout << now->tm_mday << "." << now->tm_mon + 1 << "." << now->tm_year + 1900 << endl;

    // obliczenie daty w przyszłości - 30 dni od obecnej daty
    now->tm_mday += 30;
    mktime(now);

    // wyświetlenie daty
    cout << now->tm_mday << "." << now->tm_mon + 1 << "." << now->tm_year + 1900 << endl;

    return 0;
}
```

Output:
```
29.10.2020
29.12.2020
```

## Głębsza analiza

1. Kontekst historyczny: Obliczanie daty w przeszłości lub przyszłości było niegdyś zadaniem wymagającym dużego nakładu pracy i obliczeń. Jednak dzięki rozwojowi technologicznemu i narzędziom, takim jak biblioteka <ctime>, programiści mogą wykonać to zadanie szybko i łatwo.

2. Alternatywy: Istnieje wiele alternatywnych sposobów na obliczanie daty w przeszłości lub przyszłości, takich jak użycie biblioteki Boost.Date_Time lub funkcji języka SQL. Wybór metody zależy od potrzeb i preferencji programisty.

3. Szczegóły implementacyjne: W przykładowym kodzie użyto funkcji mktime() do przeliczenia daty na ilość sekund oraz struktury tm do przechowywania informacji o dacie. Należy również zwrócić uwagę na fakt, że miesiące są numerowane od 0, a nie od 1, dlatego przy wyświetlaniu daty należy dodać 1 do wartości tm_mon.

## Zobacz też

- <time.h> - biblioteka C zawierająca funkcje do manipulacji czasem
- <ctime> - biblioteka C++ zawierająca funkcje do manipulacji czasem
- Boost.Date_Time - biblioteka C++ do obliczania i manipulacji datami
- Funkcje daty i czasu w języku SQL - alternatywny sposób na obliczanie daty w bazach danych