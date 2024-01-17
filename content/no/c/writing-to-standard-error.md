---
title:                "Skriving til standard feil"
html_title:           "C: Skriving til standard feil"
simple_title:         "Skriving til standard feil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard error handler er en måte for programmerere å gi ut feilmeldinger eller annen viktig informasjon til brukeren. Dette gjøres vanligvis i tilfeller der programmet må avsluttes på grunn av en feil.

## Hvordan:
```
#include <stdio.h>

int main() {
    fprintf(stderr, "Oops! Det har oppstått en feil.");
    return 1;
}
```
**Output:**
```
Oops! Det har oppstått en feil.
```

## Dykk dypere:
Å skrive til standard error er en metode som har vært i bruk siden den første versjonen av C. Det finnes også en annen måte å skrive ut feilmeldinger på, ved å bruke funksjonen ```printf```. Men ved å skrive til standard error kan man gi ut separate meldinger til standard output og standard error, noe som kan være nyttig i feilhåndtering.

Implementeringen av standard error handler kan variere avhengig av operativsystem og kompiler. Noen operativsystemer tillater til og med å omdirigere standard error til en annen enhet, som for eksempel en loggfil.

## Se også:
- [fprintf documentation](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Standard error handler in C](https://stackify.com/standard-error-handler-in-c/)