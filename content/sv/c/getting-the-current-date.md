---
title:                "Hämta aktuell datum"
html_title:           "C: Hämta aktuell datum"
simple_title:         "Hämta aktuell datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumet i ditt C-program kan vara användbart i många situationer, som att visa när programmet senast kördes eller jämföra datum för att utföra viss åtgärd.

## Så här gör du

Först måste vi inkludera en header-fil som heter `time.h` i vårt program. Denna header-fil innehåller funktioner för datum och tidshantering.

```
#include <time.h>
```

Sedan behöver vi definiera en variabel av typen `time_t`, vilket är den typ som används för att hålla datum och tid i C-program. Sedan kör vi `time()` för att få den aktuella tiden i sekunder.

```
time_t curr_time;
time(&curr_time);
```

Nu kan vi använda funktionen `localtime()` för att konvertera `time_t`-variabeln till en struktur som innehåller den aktuella lokala tiden. Vi kan också använda `strftime()` för att formatera datumet enligt våra önskemål.

```
struct tm *local_time = localtime(&curr_time);

char buffer[80];
strftime(buffer, 80, "%d/%m/%Y", local_time);

printf("Idag är det: %s", buffer);
```

Output:

```
Idag är det: 14/10/2021
```

## Djupdykning

I vårt exempel använde vi `strftime()` för att formatera datumet enligt vår preferens. Det finns dock många andra sätt att formatera datumet i C-programmering.

`localtime()` returnerar en struktur som innehåller alla element som behövs för att representera datum och tid, som dag, månad, år, timmar, minuter osv. Du kan utforska mer om dessa element och använda dem enligt dina behov.

För mer avancerade användningsområden, kan du också utforska andra funktioner i `time.h`-header-filen som `mktime()` och `difftime()`.

## Se även

- [Time functions in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [The beauty of strftime() in C](https://www.codingame.com/playgrounds/2487/c---time-date-and-time-tutorials/the-beauty-of-strftime-in-c)