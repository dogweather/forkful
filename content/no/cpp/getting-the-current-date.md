---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:38.234887-07:00
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Hente nåværende dato i programmer: enhver funksjon som gir oss dagens dato. Vi gjør dette for å tidsstemple hendelser, opprette rapporter eller sjekke gyldighetsperioder.

## How to (Slik gjør du det):
I C++ bruker vi `<chrono>` biblioteket for å få nåværende dato. Her er et raskt eksempel:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Får nåværende tidspunkt som time_point
    auto nå = std::chrono::system_clock::now();

    // Konverterer time_point til en time_t objekt for å representere kalendertid
    std::time_t nå_tid = std::chrono::system_clock::to_time_t(nå);

    // Konverterer time_t til en lesbar streng 
    std::cout << "Nåværende dato: " << std::ctime(&nå_tid);

    return 0;
}
```

Sample Output:
```
Nåværende dato: Wed Mar 10 11:21:54 2021
```

## Deep Dive (Dypdykk):
Historisk sett har C++ brukt `<ctime>` for dato og tid, men dette hadde sine begrensninger og komplikasjoner. Med C++11 introduserte `<chrono>` stor forbedring. Det gir type-sikkerheter og enkel bearbeidelse av tid. Alternativer inkluderer tredjepartsbiblioteker som `Boost` eller operativsystem-spesifikke kall, men `<chrono>` er standard og anbefalt.

Når vi bruker `<chrono>`, er det viktig å forstå `time_point` konseptet, som representerer et punkt i tid, og `duration`, som er en tidsintervall. Ved å konvertere til `time_t`, kan vi bruke familiære tid/dato funksjoner som `std::ctime` for å lage en strengrepresentasjon.

## See Also (Se også):
- C++ `<chrono>` documentation: https://en.cppreference.com/w/cpp/header/chrono
- C++ `<ctime>` documentation: https://en.cppreference.com/w/cpp/header/ctime
- `Boost` Date Time: https://www.boost.org/doc/libs/release/libs/date_time/
- For dybdeforståelse, se Howard Hinnant's dato og tid bibliotek som inspirerte deler av `<chrono>`: https://github.com/HowardHinnant/date
