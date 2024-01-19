---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Få tak i dagens dato med C: En guide for programmerere

## Hva & Hvordan?
Å hente dagens dato innebærer å hente informasjon om dagens år, måned og dag. Vi, programmerere, gjør dette for å holde oversikt over tid og dato i programmer, logger og rapportgenerering.

## Hvordan gjøre det:
Her er koden for hvordan du henter og skriver ut dagens dato i C:

```C
#include <time.h>
#include <stdio.h>

int main()
{
    time_t t = time(NULL);
    struct tm *local = localtime(&t);
    
    printf("Dagens dato: %02d.%02d.%d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);

    return 0;
}
```

Koden ovenfor vil generere output som er avhengig av dagens dato, for eksempel:

```
Dagens dato: 05.12.2022
```

## Dypdykk
Historisk sett har programmerere brukt forskjellige metoder for å hente dagens dato, som å bruke `gettimeofday()` eller `ctime()`. Men på grunn av bedre kompatibilitet og enkel bruk, er bruk av `time.h` bibliotek og funksjonene det kommer med, mer utbredt.

Alternativer til denne metoden kan være å bruke tredjepartsbibliotek eller kommandolinjemetoder som er spesifikke for operativsystemet, som kanskje kan gi mer presise eller varierte resultater.

Når det gjelder implementeringsdetaljer, henter `time(NULL)` det nåværende tidsstempelet, som deretter blir konvertert til lokal tid med `localtime()`. `struct tm` er en innebygd struktur i C for å holde tid og dato.

## Se også
For mer detaljert informasjon og relaterte emner, bra kilder cygwin.com/cygwin-ug-net/ntcime.html for mer om `ctime()`, og cplusplus.com/reference/ctime/ for mer om tidsbehandling i C generelt.