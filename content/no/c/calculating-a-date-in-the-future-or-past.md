---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Beregne en dato i fremtiden eller fortiden med C
---

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden er en måte å manipulere kalenderdatoer på i programmering. Det er en hendig teknikk for situasjoner som planlegging av arrangementer og tidsfristkontroll.

## Hvordan:

Her har vi et enkelt eksempel på hvordan beregne en dato i fremtiden.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm time_structure;

    time_t now = time(NULL);

    time_structure = *localtime(&now);

    time_structure.tm_mday += 7; //legger til 7 dager i dato
    mktime(&time_structure);

    char future_date[80];
    strftime(future_date, sizeof(future_date), "%d-%m-%Y", &time_structure);
  
    printf("Fremtidig dato: %s", future_date);
  
    return 0; 
}
```
Når dette programmet kjøres, vil utfallet være noe slik som:
```
Fremtidig dato: 28-09-2023
```
Det viser datoen 7 dager fra dags dato (antatt at dagens dato er 21-09-2023).

## Dyp Dykk:
Beregning av en dato i fremtiden eller fortiden har vært en nødvendig funksjon for datamaskinbruk siden de første operativsystemene. De første kodene som gjorde dette var ganske komplekse, men moderne programmeringsspråk som C har innebygde funksjoner for å gjøre denne prosessen mye enklere.

Et alternativ til dette er å bruke biblioteker som kan håndtere dato- og tidsberegning mer nøyaktig og enkelt.

Den `mktime()` funksjonen brukes til å konvertere en lokal tid, gitt som en `tm` struktur, til en `time_t` verdi med samme innhold. `mktime()` funksjonen ignorerer de opprinnelige verdiene av tm_wday og tm_yday men oppdaterer verdiene før den returnerer. 

## Se Også:
- For ytterligere læring, les [C library to handle dates](http://www.cplusplus.com/reference/ctime/)
- Se også [Date and time manipulation in C](https://www.geekhideout.com/ctime.shtml) for å utdype kunnskapen om emnet.