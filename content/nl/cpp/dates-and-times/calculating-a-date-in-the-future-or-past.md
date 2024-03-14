---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:25.582162-07:00
description: "Een datum in de toekomst of het verleden berekenen betekent uitzoeken\
  \ welke datum het zal zijn na of voor een bepaalde tijdsspanne. Het is nuttig voor\
  \ het\u2026"
lastmod: '2024-03-13T22:44:51.126104-06:00'
model: gpt-4-0125-preview
summary: "Een datum in de toekomst of het verleden berekenen betekent uitzoeken welke\
  \ datum het zal zijn na of voor een bepaalde tijdsspanne. Het is nuttig voor het\u2026"
title: Een datum in de toekomst of het verleden berekenen
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum in de toekomst of het verleden berekenen betekent uitzoeken welke datum het zal zijn na of voor een bepaalde tijdsspanne. Het is nuttig voor het creëren van herinneringen, het instellen van vervaldatums, het plannen van evenementen of simpelweg het loggen van hoeveel tijd er is verstreken.

## Hoe:
C++20 introduceerde de `<chrono>` bibliotheek upgrades, waardoor omgaan met tijd minder lastig is. Hier is een snel voorbeeld van het toevoegen van dagen aan de huidige datum:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std::chrono;

int main() {
    // De datum van vandaag ophalen
    auto today = floor<days>(system_clock::now());
    
    // 30 dagen toevoegen aan vandaag
    auto future_date = today + days(30);
    
    // Converteren naar time_point om te gebruiken met system_clock voor uitvoer
    auto tp = system_clock::time_point(future_date);
    
    // Uitvoer
    std::cout << "De datum van vandaag: "
              << std::format("{:%F}\n", today);
    std::cout << "Toekomstige datum (30 dagen later): "
              << std::format("{:%F}\n", tp);
    return 0;
}
```

Voorbeelduitvoer:
```
De datum van vandaag: 2023-03-15
Toekomstige datum (30 dagen later): 2023-04-14
```

Dagen aftrekken werkt op een vergelijkbare manier—je gebruikt dan `-` in plaats van `+`.

## Verdieping
Voor C++20 zou je misschien een bibliotheek zoals Boost gebruiken om met datums om te gaan. Maar de bijgewerkte `<chrono>` vereenvoudigt dit met `system_clock`, `year_month_day` en `duration` types.

Historisch gezien was het berekenen van datums complex vanwege het handmatig omgaan met verschillende maandlengtes, schrikkeljaren en tijdzones. C++20's `<chrono>` pakt deze problemen aan door kalender- en tijdzone-ondersteuning te bieden.

Alternatieven? Je zou nog steeds Boost kunnen gebruiken of zelfs je eigen datumlogica bedenken (avontuurlijk, maar waarom?). Er zijn ook externe bibliotheken zoals Howard Hinnant's "date" bibliotheek, welke invloedrijk was in de C++20 chrono-updates.

Wat implementatie betreft, definieert `<chrono>` duuraties als compile-time rationale constanten, waardoor problemen met floating-point vermeden worden. Types zoals `year_month_day` steunen op `sys_days`, wat een time_point vertegenwoordigt als dagen sinds een gemeenschappelijk tijdperk (1970-01-01).

## Zie ook
- C++ Referentie voor `chrono`: https://en.cppreference.com/w/cpp/header/chrono
- Howard Hinnant's Date Bibliotheek (een voorloper van de C++20 chrono-updates): https://github.com/HowardHinnant/date
- Boost Datum/Tijd documentatie: https://www.boost.org/doc/libs/release/libs/date_time/
