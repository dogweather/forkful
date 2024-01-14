---
title:    "C++: Porównywanie dwóch dat"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat może być ważną częścią codziennego programowania w języku C++. Często musimy sprawdzić, czy dwie daty są równe lub której data jest wcześniejsza lub późniejsza. W tym artykule dowiesz się, jak wykonać te porównania i jak rozwiązać ewentualne problemy.

## Jak to zrobić

Możemy porównywać daty w C++ za pomocą operatorów logicznych lub funkcji bibliotecznej `difftime()`. Poniżej znajdują się przykłady kodu i wyjścia dla obu sposobów.

### Porównywanie dat za pomocą operatorów logicznych

```C++
#include <iostream>
using namespace std;

int main() {
    // Dodajemy dwie daty do programu
    int day1 = 14;
    int month1 = 6;
    int year1 = 2020;

    int day2 = 17;
    int month2 = 6;
    int year2 = 2020;

    // Porównujemy daty za pomocą operatorów logicznych
    if (year1 == year2 && month1 == month2 && day1 == day2) {
        cout << "Daty są równe";
    } else if (year1 <= year2 && month1 <= month2 && day1 < day2) {
        cout << "Data 1 jest wcześniejsza";
    } else {
        cout << "Data 2 jest wcześniejsza";
    }

    return 0;
}
```

Wyjście: Data 1 jest wcześniejsza

### Porównywanie dat za pomocą funkcji `difftime()`

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Tworzymy dwa obiekty typu time_t
    time_t date1 = time(NULL); // Data dzisiejsza
    time_t date2 = date1 + (3*24*60*60); // Data za trzy dni

    // Obliczamy różnicę między datami w sekundach
    double difference = difftime(date1, date2);

    // Kolejne wywołanie funkcji `difftime()` zwróci wynik ujemny,
    // ponieważ data pierwsza jest wcześniejsza od drugiej
    cout << "Różnica w sekundach: " << difference << endl;

    return 0;
}
```

Wyjście: Różnica w sekundach: -259200

## Wnikliwa analiza

Porównywanie dat może być trudne, ponieważ różne kalendarze i formaty zapisu mogą powodować problemy. Pamiętaj, aby zawsze uważnie przetestować swój kod i upewnić się, że porównywane daty są w tych samych formatach.

## Zobacz też

- [Porównywanie dat w języku C++](https://pl.wikibooks.org/wiki/C_plus_plus/Por%C3%B3wnywanie_dni_tygodnia_i_dat) 
- [Dokumentacja biblioteki time.h](https://www.cplusplus.com/reference/ctime/)
- [Przydatne funkcje w bibliotece time.h](https://pl.wikibooks.org/wiki/C_interfejs_j%C4%99zyka_CPP/Zakres_czasu)