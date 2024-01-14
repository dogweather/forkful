---
title:                "C: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive en tekstfil er en vanlig oppgave i programmering. Det kan være nødvendig å lagre data eller resultatene av et program til en fil, eller å lese data fra en fil for å bruke i koden. Det kan også være nyttig for å kommunisere med andre programmer eller mellom ulike plattformer. Uansett årsak, er det viktig å forstå hvordan man skriver en tekstfil i C-programmering. 

## Hvordan du gjør det

Det første vi må gjøre er å åpne en fil ved hjelp av `fopen()`-funksjonen. Denne funksjonen tar to argumenter, filnavnet og handlingen (som for eksempel `w` for å skrive til filen). Hvis filen allerede eksisterer, vil den åpnes. Hvis ikke, vil en ny fil bli opprettet. Her er et eksempel:

```C
FILE *textfil = fopen("minfil.txt", "w");
```

Vi erklærer en peker `textfil` av typen `FILE`, og bruker `fopen()`-funksjonen for å åpne filen `minfil.txt` i skrivemodus.

For å faktisk skrive til filen, bruker vi `fprintf()`-funksjonen. Denne funksjonen tar tre argumenter: filpekeren, en formatteringsstreng og dataene vi ønsker å skrive. Her er et eksempel:

```C
fprintf(textfil, "Dette er en tekst som skal skrives til filen. %d\n", 42);
```

Vi bruker `%d`-formatet for å skrive tallet `42` til filen. Vi kan også bruke `%s` for å skrive en streng, `%f` for et flyttall, og flere andre formater. Det er viktig å lese dokumentasjonen for de ulike formatene for å sikre korrekt bruk.

Til slutt lukker vi filen ved å kalle `fclose()`-funksjonen.

```C
fclose(textfil);
```

## Dypdykk

Det er viktig å merke seg at filer må åpnes og lukkes riktig for å unngå feil og problemer. Det er også mulig å lese fra en fil ved hjelp av `fscanf()`-funksjonen, men dette vil bli dekket i en annen artikkel.

En annen viktig ting å huske på er at når man skriver til en fil, vil eksisterende data i filen bli overskrevet. Hvis man ønsker å legge til data til en eksisterende fil, kan man bruke `a` som handling i `fopen()`-funksjonen.

En interessant funksjon for å skrive flere linjer i en fil er `fputs()`. Denne funksjonen tar to argumenter, en streng og en filpeker, og skriver strengen direkte til filen. Dette kan være nyttig for å organisere data på bestemte måter i filen.

## Se også

- [Dokumentasjon for fopen()](https://www.programiz.com/c-programming/library-function/stdio.h/fscanf)
- [Eksempel på å skrive til fil](https://www.geeksforgeeks.org/write-data-file-c/)
- [Dokumentasjon for fprintf() formatstrenger](https://www.cplusplus.com/reference/cstdio/printf/)