---
title:                "C: Å skrive til standardfeil"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error i C-programmering kan være nyttig når du ønsker å sende feilmeldinger eller annen informasjon til brukeren eller logge dem for feilrapportering. Dette hjelper til med å feilsøke og debugge programmet ditt.

## Hvordan

For å skrive til standard error i C, kan du bruke funksjonen `fprintf()` og angi `stderr` som filpeker. Her er et eksempel på hvordan du kan skrive til standard error i C:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Dette er en feilmelding som skrives til standard error!\n");
    return 0;
}
```

Det forventede resultatet etter å ha kjørt dette programmet er:

```
Dette er en feilmelding som skrives til standard error!
```

## Deep Dive

Standard error er en av de tre standard strømmene i C-programmering, de to andre er standard inn og standard ut. Denne strømmen bruker vanligvis den samme plasseringen som standard ut, men den kan bli omdirigert uavhengig av standard inn og standard ut hvis det er ønskelig.

I C-programmering brukes `stderr`-variabelen som et nyttig verktøy for feilsøking og debugging, spesielt når du kjører ferdigkompilerte programmer fra terminalen. Det er også viktig å merke seg at standard error ikke blir bufferet slik som standard ut, så det som er skrevet til standard error vil vises umiddelbart.

## Se Også

- [C Programming Tutorial - Error Handling](https://www.learn-c.org/en/Error_Handling)
- [Why Use Standard Error in C?](https://www.geeksforgeeks.org/why-use-standard-error-in-c/)
- [C Programming Language - Standard Streams](https://www.programiz.com/c-programming/c-input-output)