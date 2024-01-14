---
title:    "C: Å finne lengden til en streng"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng kan være en viktig del av programmering, spesielt når man jobber med tekstbehandling. Ved å vite lengden på en streng, kan man skrive effektiv kode og unngå feil i programmet.

## Hvordan gjøre det

For å finne lengden på en streng i C, kan vi bruke funksjonen `strlen()`. Denne funksjonen tar inn en streng som parameter og returnerer lengden som et heltall.

```
C #include <stdio.h>
#include <string.h>

int main(void) {
    char string[] = "Hei, verden!";
    int lengde = strlen(string);
    printf("Lengden til strengen er: %d", lengde);
    return 0;
}
```

Output:
```
Lengden til strengen er: 13
```

Det er viktig å merke seg at funksjonen `strlen()` ikke inkluderer null-termineringen i strengen i lengden.

## Dypdykk

For å forstå hvordan `strlen()` fungerer, er det nyttig å vite litt om hvordan strenger lagres i C. En streng er egentlig bare en rekke med tegn som er lagret i minnet, og den siste karakteren er alltid null for å indikere slutten på strengen.

Når vi bruker `strlen()`, vil funksjonen loope gjennom hver karakter i strengen og telle antall tegn, helt til den når null-termineringen. Det betyr at lengden på en streng vil være en mindre enn antall tegn som faktisk er lagret i minnet.

Det finnes også andre måter å finne lengden på en streng på, for eksempel ved å bruke en `for`-løkke og telle antall tegn manuelt. Men `strlen()` er en enkel og effektiv metode som allerede er innebygd i C-biblioteket.

## Se også

- [C String Library](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [C Programming Strings](https://www.programiz.com/c-programming/c-strings)