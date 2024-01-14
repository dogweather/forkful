---
title:                "C: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man lager et program eller en applikasjon, kan det ofte være nyttig å kunne sammenligne to datoer. Dette kan for eksempel være relevant når man skal filtrere data eller utføre ulike handlinger basert på datoer. I denne bloggposten vil vi se på hvordan man kan sammenligne to datoer i C-programmering.

## Hvordan

Det finnes flere ulike måter å sammenligne datoer på i C-programmering, men den enkleste og mest nøyaktige metoden er å konvertere datoene til tidsstempel og deretter sammenligne disse. Et tidsstempel er et numerisk verdien som representerer antall sekunder siden en gitt dato og tid. For å gjøre dette, trenger vi å bruke noen innebygde funksjoner og datastrukturer i C, som `time_t` og `struct tm`.

En enkel kode for å sammenligne to datoer i C kan se slik ut:

```C
#include <stdio.h>
#include <time.h>

// Definerer en funksjon for å konvertere en gitt dato til tidsstempel
time_t to_timestamp(int dag, int måned, int år) {
    struct tm tid;
    time_t tidsstempel;

    tid.tm_mon = måned - 1; // Månedene i C starter på 0
    tid.tm_mday = dag;
    tid.tm_year = år - 1900; // Årstall i C er basert på år 1900
    tid.tm_hour = 0;
    tid.tm_min = 0;
    tid.tm_sec = 0;
    tid.tm_isdst = -1; // Setter sommer/vintertid til automatisk

    tidsstempel = mktime(&tid); // Konverterer til tidsstempel
    return tidsstempel;
}

int main() {
    // Definerer to datoer i form av dag, måned og år
    int dag1 = 25, måned1 = 6, år1 = 2021;
    int dag2 = 1, måned2 = 7, år2 = 2021;

    // Konverterer datoene til tidsstempel
    time_t dato1 = to_timestamp(dag1, måned1, år1);
    time_t dato2 = to_timestamp(dag2, måned2, år2);

    // Sammenligner datoene og printer ut resultatet
    if (dato1 > dato2) {
        printf("%d.%d.%d er senere enn %d.%d.%d\n", dag1, måned1, år1, dag2, måned2, år2);
    } else if (dato1 < dato2) {
        printf("%d.%d.%d er tidligere enn %d.%d.%d\n", dag1, måned1, år1, dag2, måned2, år2);
    } else {
        printf("%d.%d.%d er lik %d.%d.%d\n", dag1, måned1, år1, dag2, måned2, år2);
    }

    return 0;
}
```

Kjører man denne koden, vil man få følgende output:

```
25.6.2021 er tidligere enn 1.7.2021
```

## Dypdykk

Når man konverterer datoer til tidsstempel, er det viktig å være oppmerksom på at dette kun vil fungere for datoer innenfor en viss tidsperiode. Når man bruker `struct tm` i C, er året begrenset til å være mellom 1900 og 1999. For å kunne håndtere datoer utenfor dette tidsrommet, må man bruke en annen metode for å regne ut tidsstempel. En mulighet kan være å bruke en annen datastruktur som kan representere større verdier, som for eksempel `long long int`.

Det er også viktig å være klar over at tidsstempelene som genereres vil være basert på datoen og tiden til systemklokken på datamaskinen programmet kjører på. Dette kan føre til ulike resultater på forskjellige datamaskiner, avhengig av hvordan klokken er satt opp.

## Se også

- [Time handling in C](