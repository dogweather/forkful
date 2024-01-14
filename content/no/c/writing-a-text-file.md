---
title:    "C: Å skrive en tekstfil"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å skrive en tekstfil når du koder i C. Det kan være å spare data, lage loggfiler eller lagre konfigurasjonsinformasjon.

## Hvordan

For å skrive en tekstfil i C, må du først åpne en fil ved å bruke "fopen" funksjonen. Du må også spesifisere om du vil skrive til filen, ved å bruke "w" parameteret i fopen. Deretter kan du bruke "fprintf" funksjonen til å skrive ønsket informasjon til filen. Her er et eksempel på hvordan du kan skrive til en tekstfil:

```C
#include <stdio.h>

int main() {
  FILE * tekstfil;
  tekstfil = fopen("minfil.txt", "w");
  fprintf(tekstfil, "Dette er en tekstfil som er skrevet ved hjelp av C.\n");
  fclose(tekstfil);
  return 0;
}
```

Output:

Dette er en tekstfil som er skrevet ved hjelp av C.

## Dypdykk

Når du skriver en tekstfil i C, er det viktig å passe på at du lukker filen ved hjelp av "fclose" funksjonen. Dette vil sørge for at all data er skrevet til filen og at ressursene blir frigjort. Du kan også bruke "fputs" funksjonen for å skrive en hel streng til filen, eller "fputc" for å skrive enkelttegn.

Det er også viktig å sjekke om filen ble åpnet og lukket riktig, dette kan gjøres ved å bruke "fopen" og "fclose" i en "if" betingelse. Det anbefales også å bruke "feof" funksjonen for å sjekke om du har nådd slutten av filen mens du leser den.

## Se også

- [C-filbehandling](https://www.ntnu.no/wiki/display/programutvikling/Skrive+fil+i+kap%C3%A57)
- [Cppreference - File I/O](https://en.cppreference.com/w/c/io)
- [GeeksforGeeks - File Handling in C](https://www.geeksforgeeks.org/basics-file-handling-c/)