---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att parsa ett datum från en sträng innebär att vi omvandlar text i en viss format till ett datum objekt som datorn kan förstå och arbeta med. Programmerare gör detta eftersom det är ett effektivt sätt att manipulera och använda datumdata som ursprungligen kommer i textformat.

## Hur gör man:
Låt oss skapa ett enkelt exempel. Här kommer vi att använda `strptime` funktionen som tillhandahålls av `time.h` biblioteket i C.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char buf[255];

    strptime("2021-09-01", "%Y-%m-%d", &tm);
    strftime(buf, sizeof(buf), "%d %B, %Y", &tm);

    printf("Datum: %s\n", buf);

    return 0;
}
```
Denna kod omvandlar strängen "2021-09-01" till ett datum och skriver ut det i formatet "01 September, 2021".

## Djupdykning
Historiskt har varje språk hanterat datum och tider på sitt egna sätt. C introducerade `strptime` och `strftime` funktioner som ett standardiserat sätt att omvandla strängar till datum och vice versa. 
En populär alternativ till `strptime` är `sscanf` funktionen som tillhandahåller mer flexibilitet men kan bli komplex att använda.
Vad gäller interna detaljer om `strptime`, så konverterar den en tidsbeskrivning i form av en sträng till en tid värde i strukturen `struct tm`.

## Se även
1. [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
2. [C - Date & Time](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
3. [Time Management in C](https://www.geeksforgeeks.org/time-management-in-c/)