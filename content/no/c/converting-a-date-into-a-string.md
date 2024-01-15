---
title:                "Konvertering av en dato til en streng"
html_title:           "C: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av en dato til en streng er en vanlig oppgave som programmerere støter på når de jobber med datoer og tid i C. Det kan være nyttig å kunne representere en dato som en streng for å kunne vise den til brukere eller lagre den i en fil. 

## Hvordan

Det er flere måter å konvertere en dato til en streng i C, vi vil se på to av dem her. Først trenger vi å inkludere biblioteket <time.h> som inneholder funksjoner for å håndtere datoer og tid. Deretter kan vi bruke funksjonen `strftime()` for å konvertere en dato til en streng. La oss se på et eksempel:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Opprett en `tm` struktur som representerer 1. januar 2021 klokken 12:00
    struct tm dato = {.tm_year = 121, .tm_mon = 0, .tm_mday = 1, .tm_hour = 12, .tm_isdst = -1};

    // Bruk `strftime()` til å konvertere datoen til en strengformat
    char streng[20]; // Oppretter en array for å lagre strengen
    strftime(streng, 20, "%d.%m.%Y %H:%M", &dato);
    // `20` er størrelsen på arrayet, "%d.%m.%Y %H:%M" er formatteringen og `&dato` er datoen som skal konverteres

    // Skriv ut den konverterte datoen
    printf("Dato som streng: %s\n", streng);

    return 0; 
}
```
Dette vil produsere følgende output:
```
Dato som streng: 01.01.2021 12:00
``` 

Vi bruker her `strftime()` til å formatere datoen etter våre ønsker. Den første parameteren er en array som vil inneholde den konverterte datoen som en streng, den andre parameteren er størrelsen på arrayet, og den tredje parameteren er en formateringsstreng som bestemmer hvordan datoen skal se ut. Vi kan endre formateringen for å få en annen type streng.

En annen måte å konvertere en dato til en streng er å bruke `asctime()` funksjonen som er en del av <time.h> biblioteket. Dette er spesielt nyttig når du vil ha en standardisert strengrepresentasjon av datoen. La oss se på et eksempel:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Opprett en `tm` struktur som representerer 1. januar 2021 klokken 12:00
    struct tm dato = {.tm_year = 121, .tm_mon = 0, .tm_mday = 1, .tm_hour = 12, .tm_isdst = -1};

    // Bruk `asctime()` til å konvertere datoen til en streng
    char* asctime_dato = asctime(&dato);

    // Skriv ut den konverterte datoen
    printf("Dato som streng: %s\n", asctime_dato);

    return 0; 
}
```
Dette vil produsere følgende output:
```
Dato som streng: Fri Jan  1 12:00:00 2021
```

Her ser vi at strengen er standardisert og viser dato og klokkeslett på en forhåndsbestemt måte.

## Deep Dive

Når vi bruker `strftime()` funksjonen, kan vi også inkludere lokale språkinnstillinger i formateringsstrengen for å få en dato på ønsket språk. Dette gjøres ved å bruke `%a` for forkortet ukenavn og `%A` for det fulle ukenavnet på språket. Tilsvarende kan `%b` og `%B` brukes for månednavn.

Det er også viktig å merke seg at `asctime()` returnerer en peker til en streng som er lagret internt i funksjonen. Dette betyr at du ikke kan endre denne strengen og forvente at datoen vil end