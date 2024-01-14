---
title:    "C++: Att hämta aktuellt datum"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumet är viktigt för många olika programmeringsprojekt. Oavsett om du vill ha det för att logga händelser, för att beräkna mellanliggande dagar eller bara för att visa det i ett användargränssnitt, är det en viktig funktion att ha tillgång till. I denna bloggpost kommer vi att titta på hur du enkelt kan få den aktuella datumet i ditt C++-program.

## Hur man gör

För att få den aktuella datumet i C++, behöver vi använda oss av en funktion som heter "localtime". Detta är en standardfunktion i C++ som returnerar en struct som innehåller datumet och tiden för din lokala tidzon. Här är ett exempel på hur du kan använda den:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Skapa en variabel av typen "tm" som kommer att innehålla datumet och tiden
    struct std::tm *currTime;

    // Anropa "localtime" funktionen för att få den aktuella datumet och tiden
    std::time_t now = std::time(0);
    currTime = std::localtime(&now);

    // Visa den aktuella datumet och tiden i användbar form
    std::cout << "Aktuell datum och tid: " << currTime->tm_mon + 1 << "/" << currTime->tm_mday << "/" << currTime->tm_year + 1900 << " " << currTime->tm_hour << ":" << currTime->tm_min << ":" << currTime->tm_sec;

    return 0;
}

```

I detta exempel använder vi oss av en struct som heter "tm" för att lagra datumet och tiden. Detta är en standard C++ struct som används för att lagra datum och tider. Vi använder också funktionen "localtime" för att få det aktuella datumet och tiden och sedan använder vi oss av variablerna i vår struct för att visa det på ett användbart sätt.

En annan funktion som kan vara bra att känna till är "asctime". Denna funktion returnerar en sträng som innehåller datum och tid i ett läsbar form. Här är ett exempel på hur du kan använda den:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Anropa "asctime" funktionen för att få den aktuella datumet och tiden
    std::time_t now = std::time(0);
    char* currTime = std::asctime(std::localtime(&now));

    // Visa den aktuella datumet och tiden i strängform
    std::cout << "Aktuell datum och tid: " << currTime;

    return 0;
}

```

## Djupdykning

Som nämnts tidigare, använder vi oss av funktionen "localtime" för att få den aktuella datumet och tiden. Denna funktion returnerar en strukt vars namn är "tm". Denna struct innehåller flera olika variabler som är användbara för att visa datumet och tiden i olika format.

Några av dessa variabler inkluderar: 
- tm_sec - Sekunder (0-59)
- tm_min - Minuter (0-59)
- tm_hour - Timmar (0-23)
- tm_mday - Dag i månaden (1-31)
- tm_mon - Månad (0-11)
- tm_year - Antal år från 1900
- tm_wday - Dag i veckan (0-6)
- tm_yday - Dag i året (1-365)
- tm_isdst - Sommartid eller ej (0-1)

För att visa datumet i en snyggare form, kan dessa variabler användas tillsammans med andra utskriftsfunktioner i C++.

## Se även

Här är några användbara länkar för att lära dig mer om att få den aktuella datumet i C++: 
- [En översikt över std::tm struct](http://www.cplusplus.com/reference/ctime/tm/)
- [Mer information om localtime funktionen](http://www.cplusplus.com/reference/ctime/localtime/)
- [En djupare förståelse av tidsfunktioner i C++](https