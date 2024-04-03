---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:22.057613-07:00
description: "Att h\xE4mta det aktuella datumet i C++ \xE4r en grundl\xE4ggande uppgift\
  \ f\xF6r program som beh\xF6ver bearbeta eller visa datum baserat p\xE5 systemets\
  \ klocka. Det \xE4r\u2026"
lastmod: '2024-03-13T22:44:38.219701-06:00'
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i C++ \xE4r en grundl\xE4ggande uppgift\
  \ f\xF6r program som beh\xF6ver bearbeta eller visa datum baserat p\xE5 systemets\
  \ klocka."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur man gör:
C++ erbjuder flera sätt att få det aktuella datumet, inklusive C++-standardbiblioteket och tredjepartsbibliotek som Boost. Följande exempel visar hur man utför denna uppgift.

### Använda `<chrono>` (C++20 och senare)
C++20 introducerade fler funktioner i `<chrono>`-biblioteket, vilket gör det enkelt att få det aktuella datumet:
```cpp
#include <iostream>
#include <chrono>
#include <format> // För std::format (C++20)

int main() {
    auto nuvarande_tidpunkt = std::chrono::system_clock::now(); // Fånga den aktuella tiden
    auto nuvarande_time_t = std::chrono::system_clock::to_time_t(nuvarande_tidpunkt); // Konvertera till time_t

    // Formatera tiden till ett läsbart format
    std::cout << "Aktuellt datum: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(nuvarande_tidpunkt)) << std::endl;

    return 0;
}
```
**Exempel på utdata:**
```plaintext
Aktuellt datum: 2023-03-15
```

### Använda `<ctime>`
För programmerare som arbetar med äldre versioner av C++ eller de som föredrar det traditionella C-biblioteket:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Få aktuell tid
    std::tm* nu = std::localtime(&t);
    std::cout << "Aktuellt datum: " 
              << (nu->tm_year + 1900) << '-' 
              << (nu->tm_mon + 1) << '-'
              <<  nu->tm_mday
              << std::endl;

    return 0;
}
```
**Exempel på utdata:**
```plaintext
Aktuellt datum: 2023-03-15
```

### Använda Boost Date_Time
För projekt som använder Boost-biblioteken erbjuder Boost Date_Time-biblioteket en alternativ metod för att få det aktuella datumet:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Få den aktuella dagen med Boosts gregorianska kalender
    boost::gregorian::date idag = boost::gregorian::day_clock::local_day();
    std::cout << "Aktuellt datum: " << idag << std::endl;

    return 0;
}
```
**Exempel på utdata:**
```plaintext
Aktuellt datum: 2023-Mar-15
```
Dessa exempel tillhandahåller en grundläggande grund för att arbeta med datum i C++, avgörande för ett brett spektrum av applikationer.
