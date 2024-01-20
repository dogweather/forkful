---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Sammenligne to datoer i C

## Hva & Hvorfor?
Å sammenligne to datoer er å bestemme hvilken dato som kommer først. Programmerere gjør dette for å skape tidslinjer, sortere hendelser og administrere tidsavhengige oppgaver.

## Hvordan:
Her er en enkel måte å sammenligne to datoer på i C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = { .tm_year=2022, .tm_mon=11, .tm_mday=2};
    struct tm date2 = { .tm_year=2022, .tm_mon=9, .tm_mday=4};

    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    if (time1 < time2)
        printf("Dato1 kommer før Dato2\n");
    else if (time1 > time2)
        printf("Dato1 kommer etter Dato2\n");
    else
        printf("Datoene er de samme\n");

    return 0;
}
```
Sample output:
```
Dato1 kommer etter Dato2
```

## Dypt Dykk:
Å sammenligne datoer i programmering historisk sett krevde manuell prosessering av hver dato komponent. Med fremveksten av innebygde strukturer og funksjoner i moderne C som `time.h`, har dette blitt mer strømlinjeformet.

Alternativer inkluderer bruk av tredjepart biblioteker som gir utvidede funksjoner for kompliserte datobehandlinger.

Når det gjelder sammenligning, implementerer vi dette ved å konvertere begge datoene til UNIX-tid (antall sekunder siden 1. januar 1970) ved hjelp av `mktime` funksjonen, and så sammenligner vi de.

## Se Også:
- [Complete Time.h Library Tutorial](https://www.geeksforgeeks.org/time-h-library-in-c-with-examples/)
- [Comparing Dates: Stackoverflow](https://stackoverflow.com/questions/1676632/how-to-compare-dates-in-c)
- [Date and Time in C: TutorialsPoint](https://www.tutorialspoint.com/c_standard_library/time_h.htm)