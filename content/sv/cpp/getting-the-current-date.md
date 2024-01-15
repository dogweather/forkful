---
title:                "Att få den aktuella datumet"
html_title:           "C++: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Varför

Att få den aktuella datumen är en viktig del av många C++ program. Det kan användas för att spåra utförda åtgärder eller för att visa aktuell tid för användaren. Det är också användbart för att kontrollera och jämföra datum inom ett program.

# Hur du gör det

För att få den aktuella datumen i ett C++ program, behöver du använda funktionen `std::chrono::system_clock::now()`. Denna funktion returnerar ett `std::chrono::time_point` objekt som innehåller aktuell tid. Du kan sedan använda olika metoder för att få årtal, månad, dag, timmar, minuter och sekunder från detta objekt. Här är ett exempel på hur du kan göra det:

```C++
// Inkludera nödvändiga bibliotek
#include <iostream>
#include <chrono>

int main()
{
    // Hämta aktuell tidpunkten
    auto now = std::chrono::system_clock::now();

    // Få årtal från tidpunkten
    auto year = std::chrono::year_month_day{now}.year();

    // Få månad från tidpunkten
    auto month = std::chrono::year_month_day{now}.month();

    // Få dag från tidpunkten
    auto day = std::chrono::year_month_day{now}.day();

    // Få timmar från tidpunkten
    auto hours = std::chrono::hour{now};

    // Få minuter från tidpunkten
    auto minutes = std::chrono::minute{now};

    // Få sekunder från tidpunkten
    auto seconds = std::chrono::second{now};

    // Skriv ut den aktuella datumen
    std::cout << "Det är " << day << "/" << month << "/" << year
              << " klockan " << hours << ":" << minutes << ":" << seconds << std::endl;

    return 0;
}
```

Output av koden:

``` 
Det är 9/3/2021 klockan 12:24:53
```

# Fördjupning

För att förstå mer om `std::chrono::system_clock::now()` funktionen, måste vi förstå hur den fungerar bakom kulisserna. `system_clock` är en del av den `chrono` namespace som tillhandahåller olika tidsrelaterade funktioner och typer i C++. `now()` funktionen använder den aktuella tiden på din dator som en referenspunkt för att få det aktuella datumet. Det används vanligtvis för att få datum och tid av systemet i UTC-tidszonen. Det finns också andra klockfunktioner i `chrono` namespace som kan användas för att få olika typer av tider.

# Se även

- En guide till `chrono` namespace: https://www.cplusplus.com/reference/chrono/
- En djupdykning i `std::chrono::system_clock::now()` funktionen: https://en.cppreference.com/w/cpp/chrono/system_clock/now