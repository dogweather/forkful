---
title:                "Utskriving av feilsøkingsutdata"
html_title:           "C: Utskriving av feilsøkingsutdata"
simple_title:         "Utskriving av feilsøkingsutdata"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskrift av feilsøkingsutdata refererer til å skrive ut ekstra informasjon til konsollen for å hjelpe programmerere med å finne og fikse feil i koden. Dette gjøres for å lette feilsøkingsprosessen og gjøre det enklere å forstå hva som skjer i koden.

## Hvordan:
For å skrive ut en tekstmelding til konsollen i C, bruker vi funksjonen "printf". Dette kan gjøres på følgende måte:
```
#include <stdio.h>

int main() {
  printf("Dette er en debug melding\n");
  
  return 0;
}
```
Konsollen vil da skrive ut "Dette er en debug melding". Du kan også skrive ut variabler og verdier ved å bruke spesielle formateringsalternativer:
```
#include <stdio.h>

int main() {
  int num = 42;
  float pi = 3.14;
  
  printf("Nummeret er %d og pi er %f\n", num, pi);
  
  return 0;
}
```
Output vil være "Nummeret er 42 og pi er 3.14".

## Dypdykk:
Utskrift av debug utdata ble først introdusert i C i 1972 og har siden blitt en viktig del av feilsøkingsprosessen. Alternativene til å skrive ut til konsollen er å bruke en debugger eller logge utdata til en fil. Implementeringen av "printf" funksjonen er basert på standard C library (libc) og kan variere mellom ulike C-versjoner.

## Se Også:
- [The Joy of Debugging in C](https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/debugging.html)
- [The printf family of functions in C](https://www.geeksforgeeks.org/printffprintf-c/)