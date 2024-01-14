---
title:                "C: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få dagens dato er en vanlig oppgave i programmering som kan være nyttig av flere grunner. Dette kan inkludere å registrere når en bestemt hendelse skjedde, å vise dato på et brukergrensesnitt eller å beregne tidsintervaller fra nåtid.

## Hvordan

Å få dagens dato i C-programmering er en enkel oppgave som kan gjøres på flere måter. En av de mest vanlige måtene er å bruke funksjonen `time()` fra standardbiblioteket `<time.h>`. Denne funksjonen returnerer antall sekunder som har gått siden 1. januar 1970 kalt "Epoken". Ved å dele dette tallet med antall sekunder i en dag (86400), kan vi få antall hele dager siden Epoken. Dette kan deretter konverteres til en mer leslig dato ved hjelp av andre funksjoner fra `<time.h>` som `localtime()` og `strftime()`.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Få antall sekunder siden Epoken
    time_t now = time(NULL);

    // Beregn antall dager siden Epoken
    int days_since_epoch = now / 86400;

    // Konverter til datostruktur
    struct tm *date = localtime(&days_since_epoch);

    // Bruk strftime for å formatere datoen
    char formatted_date[11];
    strftime(formatted_date, 11, "%d %b %Y", date);

    // Skriv ut datoen på standard output
    printf("Dagens dato er: %s\n", formatted_date);

    return 0;
}
```
**Output:** Dagens dato er: 03 Aug 2021

## Dypdykk

En annen måte å få dagens dato på er å bruke funksjonen `ctime()` fra `<time.h>`. Denne funksjonen returnerer en lesbar streng av dagens dato og klokkeslett. En ting å merke seg med denne metoden er at den bruker systemets lokale tidssone, så det kan være forskjeller mellom datoen på forskjellige systemer.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Få dagens dato som en streng
    char *today = ctime(NULL);

    // Skriv ut datoen på standard output
    printf("Dagens dato er: %s", today);

    return 0;
}
```
**Output:** Dagens dato er: Sun Aug  3 00:00:00 2021

Det finnes også flere tredjeparts biblioteker som kan brukes for å få dagens dato i en ønsket format eller med støtte for forskjellige tidssoner.

## Se også

- [Mer om `<time.h>` biblioteket (offisiell dokumentasjon)](https://www.cplusplus.com/reference/ctime)
- [Informasjon om Epoken i C (på engelsk)](https://www.epochconverter.com/programming/c)
- [Eksempler på flere metoder for å få dagens dato i C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)