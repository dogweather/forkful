---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C++: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Beregne en dato i fremtiden eller fortiden i C++

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er prosessen med å legge til eller trekke fra dager, måneder, eller år til en eksisterende dato. Dette kan være nyttig for programmerere for å administrere oppgaver som eventplanlegging, aldersverifisering eller fristkontroll.

## Hvordan:
Her er et enkelt C++ eksempel på hvordan beregne en fremtidig dato:
```C++
#include <iostream>
#include <chrono>

int main() {
  std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
  std::chrono::system_clock::time_point next_month = now + std::chrono::hours(24 * 30);

  std::time_t time_now = std::chrono::system_clock::to_time_t(now);
  std::time_t time_next_month = std::chrono::system_clock::to_time_t(next_month);

  std::cout << "Nåværende dato: " << std::ctime(&time_now);
  std::cout << "Dato om en måned: " << std::ctime(&time_next_month);

  return 0;
}
```
Output vil da være:
```C++
Nåværende dato: Tue Oct 13 12:00:00 2021
Dato om en måned: Thu Nov 12 12:00:00 2021
```

## Dybdeplunge
Historisk sett har dato- og tidsberegninger vært en vanskelig oppgave på grunn av kompleksiteten i kalendersystemene. Moderne programmeringsspråk som C++ har innebygde biblioteker som `chrono` som hjelper med disse beregningene.

Et alternativ til `chrono` ville være eldre C++ tid og dato biblioteker som `ctime`. Men, `chrono` gir mer presise og fleksible metoder for dato- og tidsberegninger og er å foretrekke i moderne C++ programmering.

En ting å merke seg ved beregning av en dato i fremtiden eller fortiden er håndtering av datoer over månedsgrenser, årsskifter og skuddår, som alle kan påvirke nøyaktigheten av beregningene.

## Se Også:
1. C++ chrono bibliotek: h[ttps://en.cppreference.com/w/cpp/chrono](https://en.cppreference.com/w/cpp/chrono)
2. Tid og dato i C++: [https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
3. C++20 Dato bibliotek: [https://en.cppreference.com/w/cpp/chrono](https://en.cppreference.com/w/cpp/chrono)