---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:10.499195-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
("Hva & Hvorfor?")
Å hente gjeldende dato i C betyr å finne ut den nøyaktige datoen akkurat nå. Programmerere gjør dette for funksjonalitet som logger, tidsstempel og tid-sensitive operasjoner.

## How to:
("Hvordan gjøre det:")
Nedenfor finner du en enkel kode for å hente og vise gjeldende dato:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    printf("Dagens dato er: %02d-%02d-%d\n",
           tm->tm_mday, tm->tm_mon + 1, tm->tm_year + 1900);
    return 0;
}
```

Når du kjører denne koden, får du noe sånt:
```
Dagens dato er: 24-03-2023
```

## Deep Dive:
("Dypdykk")
Historisk sett har håndtering av datoer og klokkeslett i C ikke forandret seg mye. `time.h` biblioteket har vært standarden siden C ble til. Andre biblioteker som `sys/time.h` gir noe høyere presisjon og funksjoner, men `time.h` dekker de grunnleggende behovene.

Det finnes alternative måter å hente datoer på, som POSIX funksjoner `gettimeofday` og tidssoner håndtert av `gmtime`, men for mange behov er `localtime` mer enn nok.

Når du henter den lokale tiden med `localtime`, tolkes tiden som om den er i systemets lokale tidssone. Dette kan være viktig å vurdere om programmet ditt skal fungere i flere tidssoner.

## See Also:
("Se også")
For mer informasjon, se den offisielle dokumentasjonen for `time.h`:
- https://en.cppreference.com/w/c/chrono

Introduksjoner til tidshåndtering i C:
- https://www.tutorialspoint.com/c_standard_library/time_h.htm
- https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/

Å lære mer om tidssoner og `gmtime`:
- https://man7.org/linux/man-pages/man3/gmtime.3.html
