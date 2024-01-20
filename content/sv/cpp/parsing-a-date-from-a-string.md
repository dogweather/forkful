---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:35:23.429810-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att tolka ett datum från en sträng innebär att extrahera och omvandla textinformation till ett datumformat som programmet kan hantera. Utvecklare gör detta för att möjliggöra bearbetning av datumdata som kommer i textform, t.ex. från användarinmatning eller filer.

## How to:
I C++, kan vi använda `<chrono>` biblioteket tillsammans med `<sstream>` och `<iomanip>` för att tolka datumsträngar.

```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-03";  // YYYY-MM-DD format
    std::istringstream iss(date_str);
    std::chrono::system_clock::time_point tp;
    iss >> std::chrono::parse("%Y-%m-%d", tp);

    if (iss.fail()) {
        std::cout << "Parse failed\n";
    } else {
        std::cout << "Parse succeeded\n";
        // Gör något med 'tp' nu...
    }
    return 0;
}
```
Sample output:
```
Parse succeeded
```

## Deep Dive
Förr, hade C++ utvecklare ofta behövt luta sig mot bibliotek som `<ctime>` och funktioner som `strptime` för att bearbeta datumsträngar. Med introduktionen av `<chrono>` i C++11 och sedan utökningar i C++20, erbjuder C++ standardbiblioteket nu mer robusta och säkra verktyg för datum- och tidsbearbetningar. Trots dessa förbättringar så kan tredjepartbibliotek som Boost.Date_time eller Howard Hinnant's date bibliotek fortfarande vara till hjälp för mer komplexa behov.

När du tolkar datum från strängar, tänk på formatet som datumsträngarna kommer i. `<chrono>` hanterar många standardformat men är strikt; om strängen avviker från förväntat format kan parsningen misslyckas.

## See Also
- C++ `chrono` documentation: https://en.cppreference.com/w/cpp/chrono
- Howard Hinnant's date library: https://github.com/HowardHinnant/date
- Boost.Date_time dokumentation: https://www.boost.org/doc/libs/release/libs/date_time/