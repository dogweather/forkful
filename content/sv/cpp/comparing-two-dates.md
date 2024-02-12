---
title:                "Jämföra två datum"
aliases:
- sv/cpp/comparing-two-dates.md
date:                  2024-01-20T17:32:37.370900-07:00
model:                 gpt-4-1106-preview
simple_title:         "Jämföra två datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Att jämföra två datum innebär att fastställa om de är lika, vilket är tidigare, eller vilket är senare. Programmerare gör detta för att hantera tidsbaserade händelser, giltighetsperioder, schemaläggning eller uppgifter som är beroende av tid och datum.

## How to:
Använd `std::chrono` biblioteket för att hantera datum och jämför dem med enkelhet:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    system_clock::time_point today = system_clock::now();
    system_clock::time_point tomorrow = today + hours(24);

    if (today < tomorrow) {
        std::cout << "Today is before tomorrow.\n";
    } else {
        std::cout << "Today is not before tomorrow.\n";
    }

    return 0;
}
```
Exempelutskrift:
```
Today is before tomorrow.
```

## Deep Dive:
I de tidiga dagarna av C++, jämförde vi datum med `time_t` och `tm` strukturer. Med `std::chrono` introducerat i C++11 och förbättrat i senare versioner, blev datum- och tidshantering mer intuitivt och typsäkert.

Alternativ för datumjämförelse inkluderar tredjepartsbibliotek som Boost.Date_Time. Men `std::chrono` räker ofta till och har fördelen att vara inbyggt och standardiserat.

Detaljerna i implementering är att vi använder tidsklockor som `system_clock` för att få nuvarande tidsinstans, och därmed skapar tidsobjekt. Genom att använda en "time_point" kan vi sedan använda jämförelseoperatorer för att se hur dessa punkter relaterar till varandra.

## See Also:
Läs vidare i officiella dokumentationen:
- [std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [C++ Date and Time tutorial](https://www.cplusplus.com/reference/ctime/)

Utforska alternativa bibliotek:
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)

Förstå fler exempel och användningsfall:
