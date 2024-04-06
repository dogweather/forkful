---
date: 2024-01-20 17:32:34.049332-07:00
description: "How to: F\xF8r `<chrono>` biblioteket ble introdusert med C++11, brukte\
  \ C++ programm\xF8rer `<ctime>`. I dag tilbyr `<chrono>` en moderne og type-sikker\u2026"
lastmod: '2024-04-05T22:50:55.120040-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r `<chrono>` biblioteket ble introdusert med C++11, brukte C++ programm\xF8\
  rer `<ctime>`."
title: Sammenlikning av to datoer
weight: 27
---

## How to:
```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t now = std::time(nullptr);
    std::tm* now_tm = std::localtime(&now);

    std::tm other_date = *now_tm;
    other_date.tm_mday += 7; // La oss si vi legger til 7 dager

    // Sammenlign datoer
    double seconds_diff = std::difftime(std::mktime(&other_date), now);
    if (seconds_diff == 0) {
        std::cout << "Datoene er identiske.\n";
    } else if (seconds_diff > 0) {
        std::cout << "Andre dato kommer etter den første.\n";
    } else {
        std::cout << "Andre dato er før den første.\n";
    }

    return 0;
}
```
Sample output:
```
Andre dato kommer etter den første.
```

## Deep Dive
Før `<chrono>` biblioteket ble introdusert med C++11, brukte C++ programmører `<ctime>`. I dag tilbyr `<chrono>` en moderne og type-sikker tilnærming til dato- og tidshåndtering.

Alternativer inkluderer tredjeparts biblioteker som Boost.DateTime, men C++ sitt standardbibliotek blir stadig bedre og skal dekke de fleste behov.

Å implementere dato-sammenligning manuelt kan være risikabelt grunnet komplikasjonene med skuddår, tidssoner og historiske dato-forskjeller. Det er best å bruke etablerte biblioteker som ordner disse detaljene for deg.

## See Also
- [cppreference.com on <chrono>](https://en.cppreference.com/w/cpp/chrono)
- [The Boost.DateTime documentation](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
