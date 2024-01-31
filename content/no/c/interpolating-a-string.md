---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:50:18.879335-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strenginterpolasjon lar oss sette inn variabler direkte i tekststrenger. Det er kjekt for å bygge dynamisk tekst, som brukergrensesnitt eller loggmeldinger.

## Hvordan gjøre det:
Kodeeksempler og utdata ser slik ut:

```C
#include <stdio.h>

int main() {
    int alder = 30;
    char *navn = "Ola";

    printf("Hei, jeg heter %s og jeg er %d år gammel.\n", navn, alder);

    return 0;
}
```
Utdatat blir:
```
Hei, jeg heter Ola og jeg er 30 år gammel.
```

## Dypdykk
Strenginterpolasjon er ikke en egen funksjon i C, men det ligner på bruk av `printf` for å blande tekst og variabler. Historisk sett hadde C ingen innebygd måte for strenginterpolasjon, i motsetning til nyere språk som Python. Alternativer inkluderer å bruke sprintf for å skrive til en streng, eller strcat for å slå sammen strenger. Den viktigste implementeringsdetaljen er å forstå format spesifikatorer (som `%s` for strenger og `%d` for heltall) som brukes sammen med `printf`.

## Se også
- C Standard Library Documentation: https://en.cppreference.com/w/c/io/fprintf
- Format Specifiers: https://www.cplusplus.com/reference/cstdio/printf/
- "Effective C": An Introduction to Professional C Programming, No Starch Press
