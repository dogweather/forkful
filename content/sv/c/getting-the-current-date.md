---
title:    "C: Att få aktuellt datum"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet är en viktig del av många C-programmering projekt. Oavsett om det är för att visa datumet i en applikation eller för att utföra vissa beräkningar, är det viktigt att veta hur man gör detta korrekt. Läs vidare för att lära dig hur du kan implementera detta i dina C-program.

## Hur man gör

För att hämta det aktuella datumet i C-programmering, används funktionen "time.h". Det finns två steg som behövs för att implementera detta: deklarera variabeln och anropa funktionen. Här är ett exempel på C-kod för att hämta det aktuella datumet:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Deklarera variabeln för aktuellt datum
    time_t rawtime;
    struct tm * timeinfo;

    // Anropa funktionen för att hämta tiden
    time(&rawtime);
    timeinfo = localtime(&rawtime);

    // Skriv ut tiden i läsbar format
    printf("Aktuellt datum är: %s", asctime(timeinfo));

    return 0;
}
```

### Utmatning

```
 Aktuellt datum är: Sat May 8 12:21:05 2021
```

## Djupdykning
Nu när vi har sett hur man hämtar det aktuella datumet i C, låt oss ta en närmare titt på funktionen "time.h". Denna funktion innehåller flera andra användbara funktioner för datum och tid, inklusive "localtime" som används för att konvertera det råa datumet till ett mer läsbart format.

En viktig sak att notera är att funktionen "time()" returnerar tiden i antal sekunder sedan 1 januari 1970. För att konvertera detta till ett mer meningsfullt format, används "localtime()" för att dela upp den totala tiden i timmar, minuter, sekunder och datum. Om du vill ha mer detaljerad information om dessa funktioner och hur man använder dem, kan du kolla in "time.h" dokumentationen.

## Se även

- [C Time Library Documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C Programming Tutorial: Date and Time](https://www.cprogramming.com/tutorial/c/lesson15.html)
- [GeeksforGeeks: Date and Time functions in C with Examples](https://www.geeksforgeeks.org/date-time-functions-in-c-with-examples/)