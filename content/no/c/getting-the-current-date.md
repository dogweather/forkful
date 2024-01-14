---
title:    "C: Hente gjeldende dato"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget skal vi snakke om hvordan man kan få den nåværende datoen ved hjelp av programmeringsspråket C. Dette kan være nyttig for å lage programmer som trenger å vite den nåværende datoen, for eksempel for å generere rapporter eller lage kalendere.

## Hvordan

Det finnes flere måter å få den nåværende datoen på i C, men den enkleste er å bruke funksjonen `time()`. Denne funksjonen returnerer antall sekunder siden midnatt, 1. januar 1970. Ved å dele dette tallet med antall sekunder i en dag (86400), kan vi få dagens dato i antall dager siden 1970. Vi kan deretter bruke denne informasjonen til å konvertere til en lesbar dato ved hjelp av standard C-funksjoner.

Her er et eksempel på hvordan dette kan gjøres i C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Hent antall sekunder siden 1970
    time_t now = time(NULL);
    // Beregn antall dager siden 1970
    int days = now / 86400;
    // Konverter til dato
    struct tm *date = localtime(&now);
    // Skriv ut dato i formatet dd.mm.åååå
    printf("Dagens dato er: %02d.%02d.%d\n", date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);
    return 0;
}
```
Kjører man dette programmet vil man få følgende output:

```
Dagens dato er: 14.09.2021
```

Det finnes også andre måter å få den nåværende datoen på i C, som for eksempel å bruke biblioteker som `time.h` eller `ctime`. Men konseptet er det samme - å konvertere antall sekunder siden en bestemt dato til en lesbar dato.

## Dypdykk

For de som er interessert i å lære mer om hvordan man kan få den nåværende datoen i C, finnes det mange ressurser der ute. Det er viktig å forstå at måten å få den nåværende datoen på kan variere avhengig av hvilket operativsystem man bruker. Derfor er det lurt å se på dokumentasjonen for det spesifikke systemet man jobber på.

En annen ting å merke seg er at tiden som returneres av `time()` funksjonen ikke nødvendigvis er i den lokale tidszonen. Det kan være avhengig av innstillingene på datamaskinen. Derfor må man være forsiktig med å bruke denne verdien direkte for å beregne datoer i ulike tidszoner.

## Se også

- [How to Get Current Date and Time in C](https://www.programiz.com/c-programming/library-function/time)
- [How to Convert Seconds to Date and Time in C](https://www.tutorialspoint.com/How-to-Convert-Seconds-to-Date-and-Time-in-C-language)
- [C Date and Time Functions](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)