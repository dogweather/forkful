---
title:                "C: Å få gjeldende dato"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å ville vite den gjeldende datoen i et C-program. Kanskje du trenger å vise datoen til brukeren, sjekke om en dato er innenfor et bestemt tidsintervall, eller til og med sørge for at en applikasjon fungerer riktig på ulike datoer. Uansett hva årsaken er, er det viktig å kunne hente den gjeldende datoen i et program, og det er akkurat det vi vil vise deg i denne bloggposten.

## Hvordan

For å hente den gjeldende datoen i et C-program, må du inkludere "time.h" header-filen. Dette gir tilgang til funksjoner som lar deg manipulere og få informasjon om dato og tid. Det finnes flere forskjellige funksjoner for å hente den gjeldende datoen, men i dette eksempelet vil vi bruke "time" funksjonen.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now;
    struct tm *currentTime;

    time(&now); // henter den nåværende tiden
    currentTime = localtime(&now); // konverterer til lokal dato og klokkeslett

    printf("Dagens dato er: %d/%d/%d", currentTime->tm_mday, currentTime->tm_mon + 1, currentTime->tm_year + 1900);
    return 0;
}
```

I dette eksempelet deklarerer vi en variabel "now" av typen "time_t", som er en kjepp til tiden. Deretter bruker vi "time" funksjonen for å hente den nåværende tiden og lagrer den i variabelen vår. Vi bruker deretter "localtime" funksjonen for å konvertere tiden til lokal dato og klokkeslett, som vi deretter kan få tilgang til ved hjelp av "struct tm" strukturen. Her kan vi bruke flere formateringsalternativer, som for eksempel å vise månedens navn i stedet for et tall. I dette eksempelet bruker vi "tm_mday" for å få dagens dato, "tm_mon" for å få måneden og "tm_year" for å få året. Det er viktig å merke seg at måneden og året er "zero-indexed", noe som betyr at vi må legge til 1 og 1900 for å få den faktiske måneden og året.

Når vi kjører dette programmet, får vi følgende utskrift:

```
Dagens dato er: 29/10/2020
```

## Dypdykk

Som nevnt tidligere, finnes det flere forskjellige funksjoner for å hente den gjeldende datoen i et C-program. En annen nyttig funksjon er "gmtime", som henter den universelle tiden og konverterer den til dato og klokkeslett i Greenwich Mean Time (GMT). Dette kan være nyttig hvis du trenger å håndtere tidssoner i programmet ditt.

Det finnes også funksjoner for å få tilgang til spesifikke deler av datoen, som for eksempel "tm_hour" for å få timen på dagen eller "tm_min" for å få minuttene. Utforsk gjerne flere av disse mulighetene og se hvordan du kan tilpasse datoen i henhold til dine behov.

## Se også

- [C Time and Date functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C Reference Manual - <time.h>](https://www.win.tue.nl/~aeb/linux/lk/lk-4.html)
- [Date and Time in C](https://www.geeksforgeeks.org/date-time-programming-in-c)