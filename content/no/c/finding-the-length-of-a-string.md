---
title:                "Å finne lengden av en streng."
html_title:           "C: Å finne lengden av en streng."
simple_title:         "Å finne lengden av en streng."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strengen (string) er en viktig datatype i C, og å finne lengden av en streng er en vanlig operasjon for programmører. Å finne lengden av en streng betyr å telle antall tegn i en streng, inkludert mellomrom og spesielle tegn. Dette er nyttig for å manipulere strenger og utføre andre operasjoner på dem.

## Slik gjør du det:
For å finne lengden av en streng i C, kan du bruke standardfunksjonen `strlen()`. Denne funksjonen tar inn en streng som parameter og returnerer lengden som et heltall. Her er et eksempel på hvordan du kan bruke `strlen()` i et program:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Dette er en streng";
  int lengde = strlen(str);
  printf("Lengden av strengen er %d\n", lengde);
  return 0;
}
```

Dette vil skrive ut følgende til terminalen:

```
Lengden av strengen er 19
```

## Dypdykk:
Historisk sett har det vært en debatt blant programmører om hvorvidt å finne lengden av en streng bør være en innebygd funksjon eller ikke. I C var det opprinnelig ikke en innebygd funksjon, men en funksjon som måtte importeres fra `<string.h>` biblioteket. Senere versjoner av C har inkludert `strlen()` som en innebygd funksjon.

I tillegg til `strlen()`, kan du også finne strengens lengde ved å bruke en løkke og telle antall tegn manuelt. Dette kan være nyttig hvis du ønsker å gjøre andre operasjoner på strengen samtidig som du finner lengden.

Implementasjonsmessig, bruker `strlen()` en peker og sammenligner hver karakter i strengen med `\0`, som indikerer slutten på en streng. Dette fortsetter inntil funksjonen finner `\0`, og returnerer da antall iterasjoner.

## Se også:
- [Mer om strengmanipulering i C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Offisiell dokumentasjon for `strlen()`](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Alternativ måte å finne strengens lengde på](https://www.geeksforgeeks.org/write-string-copy-destination-c/)