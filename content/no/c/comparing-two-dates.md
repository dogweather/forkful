---
title:                "Sammenligning av to datoer"
html_title:           "C: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Så, du lurer kanskje på hvorfor programmerere er alltid snakker om å sammenligne to datoer? Det er ganske enkelt - sammenligning av datoer er en vanlig oppgave i programmering, spesielt når det kommer til å sortere eller filtrere data basert på datoer. Det er viktig å kunne identifisere om en dato er før eller etter en annen, eller om de er like, for å sikre riktig og nøyaktig behandling av data.

## Hvordan:
Det er flere måter å sammenligne to datoer på i C-språket. Her er et eksempel på å sammenligne to datoer ved hjelp av funksjonen `difftime()`:
```C
#include <stdio.h>
#include <time.h>
int main() {
    time_t t1, t2;
    struct tm date1, date2;

    // Fyller struct med to datoer
    date1.tm_year = 2020;
    date1.tm_mon = 6;
    date1.tm_mday = 1;
    date2.tm_year = 2019;
    date2.tm_mon = 6;
    date2.tm_mday = 1;

    // Konverterer struct til time_t
    t1 = mktime(&date1);
    t2 = mktime(&date2);

    // Sammenligner datoene og skriver ut resultatet
    double result = difftime(t1, t2);
    if (result > 0) {
        printf("Dato 1 er senere enn dato 2.");
    } else if (result < 0) {
        printf("Dato 2 er senere enn dato 1.");
    } else {
        printf("Dato 1 og dato 2 er like.");
    }

    return 0;
}
```

Dette eksemplet viser hvordan man kan bruke `difftime()` til å sammenligne to datoer ved å konvertere dem til `time_t` og deretter beregne differansen mellom dem i antall sekunder.

## Dypdykk:
Historisk sett har sammenligning av datoer vært et problem for mange programmeringsspråk, men i C-språket finnes det flere innebygde funksjoner som hjelper til med å håndtere dette. I tillegg til `difftime()` finnes det også `mktime()` og `difftime()` som kan brukes til å konvertere og sammenligne datoer. Alternativt kan man også bruke `time.h` biblioteket til å utføre mer avanserte beregninger med datoer.

## Se også:
For mer informasjon om hvordan man sammenligner datoer i C, kan du sjekke ut disse ressursene:
- [Dato- og tidsfunksjoner i C](https://www.tutorialspoint.com/c_standard_library/time_h.htm) fra Tutorialspoint.
- [Sammenligning av datoer i C](https://www.geeksforgeeks.org/compare-two-dates-in-c/) fra GeeksforGeeks.