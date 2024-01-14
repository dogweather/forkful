---
title:                "C: Sammenligner to datoer."
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

For å forstå komplekse hendelser og sammenligne datoer, kan det være nyttig å kunne programmere i C. Dette kan gi deg en bedre forståelse av hvordan datering fungerer og hvordan du kan sammenligne to datoer for å trekke ut relevant informasjon.

## Hvordan

For å sammenligne to datoer i C, kan du bruke innebygde funksjoner som `difftime()` og `mktime()`. Først må du definere to `struct tm` objekter som inneholder de to datoene som skal sammenlignes. Deretter kan du bruke funksjonen `difftime()` for å beregne differansen i sekunder mellom de to datoene.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Definer to datoer
    struct tm dato1 = { .tm_year = 121, .tm_mon = 7, .tm_mday = 1 };  // 1. august 2021
    struct tm dato2 = { .tm_year = 120, .tm_mon = 6, .tm_mday = 1 };  // 1. juli 2020

    // Bruk difftime() for å beregne differansen i sekunder
    double differanse = difftime(mktime(&dato1), mktime(&dato2));

    // Skriv ut differansen i antall år
    printf("Differansen i år er %.2f", differanse / (365*24*60*60));

    return 0;
}
```

Output:
```
Differansen i år er 1.00
```

## Dypdykk

For å sammenligne to datoer i C, må du forstå hva som faktisk skjer bak kulissene. I C, representeres datoer som en posisjon på et tidslinje, vanligvis uttrykt i sekunder siden 1. januar 1970 (også kjent som "epoch"). Når du kaller `mktime()` funksjonen, konverterer den datoene til sekunder, og dermed lar deg sammenligne dem ved hjelp av `difftime()`.

Det er viktig å merke seg at C inneholder begrensninger når det gjelder hvilke typer datoer det kan håndtere. For eksempel, kan C ikke håndtere datoer før 1970, og heller ikke datoer etter år 2038, på grunn av hvordan det internasjonale tidsformatet er implementert.

## Se Også

- [Dokumentasjon for difftime()](https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html)
- [Oversikt over tid i C](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf)