---
title:    "C: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att kunna räkna ut datum i framtiden eller det förflutna är en användbar funktion i många program. Det kan till exempel användas för att skapa planeringsverktyg eller hålla reda på förloppet av händelser.

## Hur man gör

För att räkna ut datum i framtiden eller det förflutna behöver vi ett datum som utgångspunkt och ett antal dagar som ska läggas till eller dras av. Vi kan använda oss av funktionen `mktime()` tillsammans med strukturen `tm` för att hantera datum och tid i C.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm date;
    time_t newTime;

    date.tm_year = 2021 - 1900; // år 2021
    date.tm_mon = 9 - 1; // september
    date.tm_mday = 1; // den 1:a
    date.tm_hour = 0; // kl. 00:00
    date.tm_min = 0;
    date.tm_sec = 0;

    // lägger till 10 dagar
    date.tm_mday = date.tm_mday + 10;

    // konverterar till sekunder
    newTime = mktime(&date);

    // skriver ut det nya datumet
    printf("Det nya datumet är: %s", ctime(&newTime));

    return 0;
}
```

Output:

```
Det nya datumet är: Mon Sep 11 00:00:00 2021
```

## Djupdykning

När vi använder funktionen `mktime()` behöver vi konvertera vårt datum till en `struct tm`. Detta gör vi genom att använda oss av `mktime()` tillsammans med funktionen `localtime()`, som konverterar datumet till enhetlig tid och fyller i de olika fälten i `struct tm`.

För att räkna ut ett datum i framtiden behöver vi bara lägga till antalet dagar i `tm_mday` (dagen) fältet. Om detta resulterar i ett datum som inte existerar (t.ex. 31 februari), så tar `mktime()` hand om detta genom att korrekt justera datumet.

För att räkna ut ett datum i det förflutna behöver vi antingen dra bort antalet dagar från `tm_mday` fältet, eller använda oss av negativa tal i fältet för år, månad och dag.

## Se även

- [C Programming Tutorial for Beginners](https://www.programiz.com/c-programming)
- [C99 Standard Library](https://en.cppreference.com/w/c)