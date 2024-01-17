---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat jest procesem porównywania dwóch dat na podstawie ich wartości. Programiści często używają tej metody w swoich programach, aby ustalić, który dzień był wcześniejszy lub późniejszy.

## Jak to zrobić:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Tworzymy dwie zmienne typu time and date
    time_t now = time(0);
    tm *ltm = localtime(&now);

    // Ustawiamy pierwszą datę jako teraz
    cout << "Dzisiaj jest: " << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year << endl;

    // Ustawiamy drugą datę jako dwa dni później
    ltm->tm_mday += 2;

    cout << "Za dwa dni będzie: " << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year << endl;

    // Sprawdzamy, która data jest wcześniejsza
    if (difftime(mktime(ltm), now) > 0) {
        cout << "Druga data jest późniejsza." << endl;
    } else {
        cout << "Pierwsza data jest późniejsza." << endl;
    }

    return 0;
}
```

Output:
```
Dzisiaj jest: 21/6/2021
Za dwa dni będzie: 23/6/2021
Druga data jest późniejsza.
```

## Głębsza analiza

Historia porównywania dat sięga czasów starożytnych, kiedy ludzie musieli używać różnych kalendarzy do śledzenia czasu. Dziś istnieje wiele alternatywnych metod porównywania dat, takich jak funkcja `compare()` w C++ lub biblioteki zewnętrzne. Implementacja porównywania dat ma kluczowe znaczenie, aby unikać błędów i nieprawidłowych wyników.

## Zobacz także:

- [Porównywanie Dat w C++](https://www.tutorialspoint.com/cplusplus-program-to-compare-dates)
- [Funkcja Compare w C++](https://www.geeksforgeeks.org/c-program-compare-two-dates/)
- [Biblioteka Boost - porównywanie dat](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time/date_time_io.html#date_time.io_facet_compare)