---
title:                "Sammenligning av to datoer"
html_title:           "C++: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer er et vanlig scenario i mange programmeringsprosjekter. Dette kan gjøres for å sjekke om to datoer er like, eller å finne ut om en dato er før eller etter en annen.

## Slik gjør du det
For å sammenligne to datoer i C++, kan du bruke "std::chrono" biblioteket. Du må først inkludere dette biblioteket i koden din ved å legge til "#include <chrono>". Deretter kan du bruke "std::chrono::system_clock::now()" for å få gjeldende dato og tid.

```C++
#include <chrono>
using namespace std::chrono;

// Få gjeldende dato og tid
system_clock::time_point today = system_clock::now();
// Definer to datoer for sammenligning
system_clock::time_point date1 = today - hours(24);
system_clock::time_point date2 = today + hours(24);

// Sjekk om date1 er før date2
if (date1 < date2) {
    std::cout << "date1 kommer før date2";
}
```

Det er også mulig å sammenligne datoer ved å konvertere dem til "time_t" objekter med "std::chrono::system_clock::to_time_t()" og deretter sammenligne disse objektene som vanlige nummer.

```C++
// Konverter datoer til time_t objekter
time_t t1 = system_clock::to_time_t(date1);
time_t t2 = system_clock::to_time_t(date2);

// Sjekk om t1 er lik t2
if (t1 == t2) {
    std::cout << "t1 og t2 er like";
} 
```

## Dypdykk
Når du sammenligner datoer i C++, er det viktig å være klar over at de kan være av forskjellige typer. Noen typer inkluderer "system_clock", "steady_clock" og "high_resolution_clock". Disse har forskjellige egenskaper og kan påvirke hvordan datoene sammenlignes.

En annen ting å huske på er at tidenheten som brukes kan variere fra system til system, så det kan være lurt å konvertere datoer til en felles enhet for å få mer nøyaktige sammenligninger.

## Se også
- [C++ Referanse for chrono biblioteket](https://en.cppreference.com/w/cpp/header/chrono)
- [Sammenligne datoer i C++](https://www.geeksforgeeks.org/compare-two-dates-c/)
- [Datohåndtering i C++](https://www.studytonight.com/cpp/dates-and-time-in-cpp.php)