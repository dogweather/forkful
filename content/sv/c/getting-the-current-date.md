---
title:                "C: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumet är en viktig del av många C-program. Det kan hjälpa till att spåra tider och datum för användarsessioner, hantera filnamn baserat på datum och mycket mer.

## Så här gör du

För att få den nuvarande datumet i C, måste vi använda funktionen `time.h` i vårt program. Först inkluderar vi biblioteket genom att lägga till `#include <time.h>` på toppen av vårt program.

Sedan deklarerar vi en variabel `time_t current_time` för att hålla den aktuella tiden och sedan kör `time()` funktionen och tilldelar resultatet till vår variabel. Detta ger oss antalet sekunder som har gått sedan 1 januari 1970.

```C
#include <time.h>

int main() {
    time_t current_time;
    current_time = time(NULL);
    return 0;
}
```

För att få datumet från detta nummer, använder vi funktionen `localtime()` som tar innanför ett `time_t` värde och konverterar det till ett datumstrukturobjekt. Vi deklarerar även en variabel `struct tm *current_date` som håller detta datumstruktur.

```C 
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    time(&current_time);

    struct tm *current_date;
    current_date = localtime(&current_time);

    return 0;
}
```

Nu kan vi använda `current_date` för att få det aktuella datumet i formatet vi behöver. Till exempel, om vi vill skriva ut datumet i formatet "DD-MM-YYYY", kan vi använda `printf()` funktionen med ett formatsträng som innehåller olika konverteringar för dagen, månaden och året.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    time(&current_time);

    struct tm *current_date;
    current_date = localtime(&current_time);

    printf("Current Date: %02d-%02d-%04d\n", current_date->tm_mday, current_date->tm_mon+1, current_date->tm_year+1900);

    return 0;
}
```

Detta kommer att producera utmatning som "Current Date: 28-09-2020", som är dagens datum när detta skrivs.

## Djupdykning

Funktionerna i `time.h` biblioteket är baserade på Unix time system, där tiden mäts som antalet sekunder som har gått sedan 1 januari 1970. Detta är också känt som "epoch" och används för att hålla en konsekvent tidsreferenspunkt över olika system.

Den `time_t` variabel som vi använder för att lagra den aktuella tiden är egentligen bara ett heltal som representerar sekunderna som har gått sedan epoch. Detta gör det enkelt och effektivt att hantera tider i C-programmering.

## Se även

- [C Date and Time tutorial](https://www.programiz.com/c-programming/c-date-time)
- [time.h reference](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Epoch (Unix)](https://en.wikipedia.org/wiki/Epoch_(computing))