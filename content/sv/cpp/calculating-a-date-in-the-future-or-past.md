---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "C++: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett datum i framtiden eller förflutet innebär att lägga till eller dra ifrån ett specifikt antal dagar till ett givet datum. Programmerare utför denna uppgift för att spåra händelser, schemalägga påminnelser eller utföra viss tidsåtagande analys.

## Hur gör man:
Låt oss skapa en enkel C++-funktion för att lägga till ett antal dagar till dagens datum. Vi kommer att använda `<chrono>` biblioteket som innehåller tid och datum funktionaliteter.

```C++
#include <iostream>
#include <chrono>

void addDays(int days){
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point future = now + std::chrono::hours(24*days);

    std::time_t future_date = std::chrono::system_clock::to_time_t(future);

    std::cout << "Framtidens datum: " << std::ctime(&future_date);
}

int main(){
    addDays(10); //lägger till 10 dagar till dagens datum
    return 0;
}
```
Ovanstående kod kommer att ge output som detta:
```
Framtidens datum: Thu Jun 30 19:58:32 2022
```

## Djupdykning
Beräkning av datum har varit en grundläggande del av programmering sedan dagar av tidiga datorer. C++ har flera sätt att manipulera datum och tid inklusive gamla C-style `<ctime>` bibliotek och moderna `<chrono>` bibliotek.

Vissa alternativ till ovanstående metod inkluderar att använda 'Boost.Datetime' bibliotek eller 'date.h' bibliotek som ger mer robusta och flexibla metoder för datumhantering.

Effektiviteten i att beräkna datum och tid i C++ beror till stor del på detaljer som precision av tidsrepresentation och kravet på trådsäkerhet. `<chrono>` biblioteket i moderna C++-versioner erbjuder en balans mellan precision och prestanda.

## Se även
1. [Chrono library documentation](https://en.cppreference.com/w/cpp/chrono)
2. [Boost.Datetime library documentation](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
3. [Date.H library documentation](https://howardhinnant.github.io/date/date.html)

Observera att ingen del av koden i denna artikel skall tas som den definitiva lösningen på att hantera datum och tid i C++. Målet är snarare att ge dig en bra startpunkt för att utforska ämnet djupare.