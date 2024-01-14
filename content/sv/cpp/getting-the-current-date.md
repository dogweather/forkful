---
title:    "C++: Att hämta aktuellt datum"
keywords: ["C++"]
---

{{< edit_this_page >}}

##Varför

Att få den aktuella datumet är ett viktigt koncept inom programmering. Det finns många situationer då du behöver använda det aktuella datumet i dina program, såsom att skapa en loggrapport eller schemalägga uppgifter. Det är också en viktig del av att skapa användarvänliga program som visar det korrekta datumet för en specifik tidszon.

##Så här gör du

Det finns flera sätt att få den aktuella datumet i C++. Det mest grundläggande sättet är att använda `std::chrono` biblioteket, som innehåller ett uppsättning klasser och funktioner som är avsedda för tidsberäkningar. Här är ett exempel på kod som använder `std::chrono` för att få den aktuella datumet:

```C++
#include <iostream>
#include <chrono>

int main() {
    // Skapa en variabel av typen "system_clock::time_point", som representerar det aktuella datumet.
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now(); 

    // Använd "std::chrono::time_point_cast" för att konvertera "system_clock::time_point" till "time_point" för dagen.
    std::chrono::time_point<std::chrono::system_clock::duration> today = std::chrono::time_point_cast<std::chrono::days>(now);

    // Använd "std::chrono::year_month_day" för att få året, månaden och dagen från det aktuella datumet.
    auto ymd = std::chrono::year_month_day(today);

    // Skriv ut resultatet.
    std::cout << "Den aktuella datumet är: " << ymd << std::endl; 
    return 0;
}
```

Här är en möjlig utmatning för denna kod:

`Den aktuella datumet är: 2021-01-21`

Det finns också andra sätt att få den aktuella datumet, såsom att använda `localtime` biblioteket eller systemanrop. Det bästa tillvägagångssättet beror på dina specifika behov och preferenser.

##Djupdykning

När vi använder `std::chrono` för att få den aktuella datumet, använder vi egentligen inte en funktion som heter `now()` utan en klass som kallas `system_clock`. Denna klass är en grundläggande klocka som mäter tiden från en specifik punkt i det förflutna, vanligtvis den 1 januari 1970. Vi använder också `std::chrono::time_point`, som är en tidsfördröjning som består av en tidsstämpel och en "duratation" som anges av klockan.

`std::chrono::year_month_day` är en annan nyttig klass som används för att representera datum och tidpunkter. Här är några andra användbara klasser från `std::chrono`:

- `std::chrono::hours`: representerar ett intervall av timmar
- `std::chrono::minutes`: representerar ett intervall av minuter
- `std::chrono::seconds`: representerar ett intervall av sekunder

Om du vill fördjupa dig i detaljerna om hur dessa klasser fungerar, rekommenderar jag att du tittar på dokumentationen för `std::chrono` biblioteket.

##Se även

- [C++ Date and Time](https://www.cplusplus.com/reference/ctime/)
- [C++ Chrono Library](https://www.cplusplus.com/reference/chrono/)