---
title:                "Beregning av en dato i fremtiden eller fortiden"
date:                  2024-01-20T17:30:58.330241-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden betyr å finne en dato før eller etter en gitt startdato. Programmere bruker dette for å håndtere hendelser, frister, påminnelser og tidsstyring generelt.

## Hvordan til:
Lagring av tid og dato i C gjøres vanligvis med `time.h` biblioteket, som lar oss jobbe med `time_t` strukturen for å utføre tidberegninger. Her er et eksempel på hvordan man legger til en uke til gjeldende dato:

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm new_date;
    double seconds = 60 * 60 * 24 * 7; // En uke

    // Få nåværende tid og dato
    time(&now);

    // Konverter time_t til tm struktur for enkel manipulasjon
    new_date = *localtime(&now);

    // Legg til en uke
    new_date.tm_sec += seconds; 

    // Normaliser tm strukturen og konverter tilbake til time_t
    time_t one_week_from_now = mktime(&new_date);

    // Konverter til lesbar format
    printf("En uke fra nå: %s", asctime(&new_date));

    return 0;
}
```

Sample output:
```
En uke fra nå: Mon Mar 15 14:22:36 2021
```

## Deep Dive
Tid og dato i programmering har variert over tid, fra enkle tidtakere til komplekse bibliotek med tidssoner og skuddårstøtte. C har støttet tidberegninger med `time.h` siden C89/C90 standarden. Alternativer til standard C-biblioteket inkluderer POSIX `time.h` og moderne biblioteker som `date.h`.

Når vi legger til sekunder til `tm_sec`, tar `mktime` hånd om overflyt og oppdaterer de andre feltene (minutter, timer, dager, osv.) tilsvarende. Men vær forsiktig med tidsskjell og skuddsekunder; disse blir ofte håndtert av operativsystemet eller spesifikke biblioteker.

En annen ting å være klar over er håndtering av tidssoner. `localtime` bruker systemets lokale tidssone, mens `gmtime` gir Coordinated Universal Time (UTC). Valget avhenger av applikasjonens behov.

## Se Også
- `man 3 time` og `man 3 localtime` for Linux man-sider.
- C Standard Library - https://en.cppreference.com/w/c/chrono
- POSIX Programmer's Manual - https://pubs.opengroup.org/onlinepubs/9699919799/
- Howard Hinnant's Date library (for mer moderne C++ støtte) - https://github.com/HowardHinnant/date
