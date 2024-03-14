---
date: 2024-01-20 17:28:33.468710-07:00
description: "R\xE4kna ut ett framtida eller f\xF6rflutet datum handlar om att best\xE4\
  mma ett specifikt datum utifr\xE5n ett annat genom att addera eller subtrahera dagar,\
  \ m\xE5nader\u2026"
lastmod: '2024-03-13T22:44:38.222736-06:00'
model: gpt-4-1106-preview
summary: "R\xE4kna ut ett framtida eller f\xF6rflutet datum handlar om att best\xE4\
  mma ett specifikt datum utifr\xE5n ett annat genom att addera eller subtrahera dagar,\
  \ m\xE5nader\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutet"
---

{{< edit_this_page >}}

## Vad & Varför?
Räkna ut ett framtida eller förflutet datum handlar om att bestämma ett specifikt datum utifrån ett annat genom att addera eller subtrahera dagar, månader eller år. Programmerare gör detta för att hantera deadlines, boka händelser, beräkna åldrar, eller för planeringsalgoritmer.

## How to:
I C++ kan du använda `<chrono>` biblioteket som ett kraftfullt verktyg för datumhantering. Här nedan följer ett exempel på att addera en vecka till dagens datum:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    using namespace std::chrono;

    // Dagens datum
    system_clock::time_point today = system_clock::now();

    // Lägg till en vecka (7 dagar)
    system_clock::time_point next_week = today + days(7);

    // Konvertera till läsbar form
    time_t next_week_time = system_clock::to_time_t(next_week);

    // Skriv ut
    std::cout << "Idag: " << std::put_time(std::localtime(&next_week_time), "%Y-%m-%d") << std::endl;
    std::cout << "Nästa vecka: " << std::put_time(std::localtime(&next_week_time), "%Y-%m-%d") << std::endl;

    return 0;
}
```

Förväntad utdata visar dagens datum och sedan datumet för en vecka framåt.

## Deep Dive:
Före `<chrono>` blev standard i C++11, använde programmerare ofta `time.h` eller externa bibliotek som `Boost.Date_Time`. `<chrono>` introducerades för mer precision och bättre syntax. 

Förutom att bara addera dagar, kan `<chrono>` hantera tidsmätning i timmar, minuter, sekunder eller till och med nanosekunder, samt att subtrahera tiden. Implementationen använder tidsperioder (`durations`) och tidsstämplar (`time_points`), vilket möjliggör robust datum- och tidshantering.

Alternativa metoder inkluderar datumhanteringspaket i tredjepartsbiblioteket Boost eller till och med skriva en egen datumhanterare, men dessa metoder är oftast onödiga med de nuvarande standardbiblioteken.

## See Also:
- [cppreference.com/w/cpp/chrono](https://en.cppreference.com/w/cpp/chrono)
- [Boost Date_Time library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
