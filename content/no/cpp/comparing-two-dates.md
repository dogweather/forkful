---
title:                "Sammenligne to datoer"
html_title:           "C++: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer er en vanlig programmeringsoppgave for å sjekke om en dato kommer før eller etter en annen dato. Dette er nyttig for å sortere eller filtrere data eller for å utføre spesielle handlinger basert på datoer.

## Hvordan:
En enkel måte å sammenligne datoer i C ++ er å bruke ```std::chrono``` biblioteket. Følgende eksempel viser hvordan du kan sammenligne to datoer og sjekke om den ene kommer før eller etter den andre:
```
#include <iostream>
#include <chrono>

using namespace std::chrono;

int main()
{
    // Opprett to datoer
    auto birthday = year_month_day{2010_y / 8 / 10};
    auto today = year_month_day{2020_y / 4 / 15};

    // Sjekk om bursdagen kommer før eller etter i dag
    if (birthday < today) {
        std::cout << "Bursdagen din har allerede vært i år." << std::endl;
    } else {
        std::cout << "Du vil ha bursdag senere i år." << std::endl;
    }

    return 0;
}
```

Output: 
```
Bursdagen din har allerede vært i år.
```

## Dypdykk:
Før C ++ 11, var det vanlig å bruke ```ctime``` biblioteket for å håndtere datoer og tider. Dette biblioteket ble imidlertid ansett for å være tungvint og upålitelig. Med introduksjonen av ```std::chrono``` biblioteket, ble det mye enklere å håndtere datoer og tider på en nøyaktig måte.

En annen måte å sammenligne datoer på er å bruke ```time_t``` og ```difftime``` funksjonen:
```
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // Opprett to datoer
    time_t birthday = mktime(&tm{ 0, 0, 0, 8, 9, 2010 });
    time_t today = mktime(&tm{ 0, 0, 0, 4, 15, 2020 });

    // Sammenlign datoer
    if (difftime(birthday, today) < 0) {
        std::cout << "Bursdagen din har allerede vært i år." << std::endl;
    } else {
        std::cout << "Du vil ha bursdag senere i år." << std::endl;
    }

    return 0;
}
```

Output: 
```
Bursdagen din har allerede vært i år.
```

## Se også:
- [C ++ reference for dato og tid](https://en.cppreference.com/w/cpp/chrono)
- [Introduksjon til dato og tid i C ++](https://www.moderncplusplus.com/2018/09/18/introduction-to-date-and-time-in-cpp/)