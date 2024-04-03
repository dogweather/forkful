---
date: 2024-01-20 17:32:39.082920-07:00
description: "Por\xF3wnywanie dw\xF3ch dat to sprawdzenie, kt\xF3ra jest wcze\u015B\
  niejsza, czy p\xF3\u017Aniejsza, albo czy s\u0105 identyczne. Programi\u015Bci robi\u0105\
  \ to, aby zarz\u0105dza\u0107 terminami,\u2026"
lastmod: '2024-03-13T22:44:35.725283-06:00'
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dw\xF3ch dat to sprawdzenie, kt\xF3ra jest wcze\u015Bniejsza,\
  \ czy p\xF3\u017Aniejsza, albo czy s\u0105 identyczne."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## How to (Jak to zrobić)
```cpp
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    // Ustawiamy dwie daty
    std::tm date1 = {}; date1.tm_year = 120; date1.tm_mon = 4; date1.tm_mday = 15;
    std::tm date2 = {}; date2.tm_year = 121; date2.tm_mon = 4; date2.tm_mday = 15;

    // Konwersja na time_t
    std::time_t time1 = std::mktime(&date1);
    std::time_t time2 = std::mktime(&date2);

    // Porównywanie
    if (time1 < time2) {
        std::cout << "Data1 jest wcześniejsza niż Data2.\n";
    } else if (time1 > time2) {
        std::cout << "Data1 jest późniejsza niż Data2.\n";
    } else {
        std::cout << "Daty są identyczne.\n";
    }

    return 0;
}
```

### Przykładowe wyjście:
```
Data1 jest wcześniejsza niż Data2.
```

## Deep Dive (Głębsze spojrzenie)
Porównywanie dat w C++ może wykorzystywać wiele podejść. Zanim wprowadzono `<chrono>`, często korzystano z `<ctime>` i własnych funkcji. `<chrono>` daje silne typowanie i większą dokładność; od C++20 oferuje także wsparcie dla stref czasowych i kalendarzy.

Alternatywą jest również korzystanie z bibliotek zewnętrznych jak Boost.Date_Time, jeśli wymagane są bardziej zaawansowane operacje.

Ważne jest, aby zwrócić uwagę na szczegóły implementacyjne, takie jak różne sposoby przechowywania dat (np. liczba sekund od określonego punktu odniesienia, jak 'epoch'), strefy czasowe czy zmiany czasu (np. czas letni).

## See Also (Zobacz również)
- Dokumentacja C++20 `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Biblioteka Boost.Date_Time: https://www.boost.org/doc/libs/release/libs/date_time/
- Poradnik do `<ctime>`: https://www.cplusplus.com/reference/ctime/
