---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I C++ programmering er det å hente dagens dato noe som betyr å få tidsstempel som representerer det aktuelle øyeblikket. Dette er ofte nødvendig for loggføring, tidsstempling av transaksjoner, eller å håndtere tidsavhengige funksjoner.

## Hvordan:

Her er en enkel måte å få dagens dato på i C++:

```C++
#include <ctime>
#include <iostream>

int main() {
    std::time_t t = std::time(0);
    std::tm* now = std::localtime(&t);
    std::cout << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              << now->tm_mday
              << "\n";
    return 0;
}
```
Utskriften blir datoen i 'ÅÅÅÅ-MM-DD' format, som for eksempel '2022-03-06'.

## Dypdykk

Historisk sett har programmerere fått tak i systemets lokale tid for å få den nåværende datoen. I eldre C++, brukte vi 'time.h' biblioteket. Det fungerer fortsatt i dag, men den moderne C++ versjonen anbefaler 'ctime'.

Som alternativer er det biblioteker som Chrono (fra C++11 og videre) eller tredjeparts biblioteker som Date eller Boost. Disse gir mer robuste funksjoner for dato og tid håndtering.

I vårt eksempel bruker vi std::time for å hente antall sekunder siden epoch (Midnatt UTC, 1. januar 1970). Så bruker vi std::localtime til å konvertere det til en tm struct, som holder individuelle komponenter av tid (time, min, sec) og dato (day, month, year).

## Se Også

- Offisiell C++ dokumentasjon [ctime](http://www.cplusplus.com/reference/ctime/)
- Krono biblioteket [Chrono](https://en.cppreference.com/w/cpp/chrono)
- Boost bibliotekets tidsfunksjoner [Boost](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- Tredje-part biblioteket [Date](https://github.com/HowardHinnant/date)