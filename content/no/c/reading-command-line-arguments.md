---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "C: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor
Ved hjelp av kommandolinjeargumenter kan du gi programmet ditt ulike innganger hver gang det kjører, noe som gjør det mer fleksibelt og tilpasset ulike behov. Enten du er en erfaren C-programmerer eller nybegynner, kan det være nyttig å vite hvordan du leser kommandolinjeargumenter i koden din.

# Hvordan
For å lese kommandolinjeargumenter i C, må du inkludere "stdio.h"-biblioteket i koden din. Deretter kan du bruke "argc" og "argv" variabler til å få tilgang til argumentene som er gitt ved kjøring av programmet.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Antall argumenter: %d\n", argc);

  // Skriv ut alle argumentene som er gitt ved kjøring av programmet
  for (int i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }

  return 0;
}
```

For eksempel, ved å kjøre programmet med "gcc program.c -o program" og "program arg1 arg2", vil du få følgende utgang:

```
Antall argumenter: 3
Argument 0: program
Argument 1: arg1
Argument 2: arg2
```

Du kan også lese enkeltargumenter ved å bruke indeksering på "argv" variabelen.

```C
// Leser kun det første argumentet som ble gitt
char *argument = argv[1];
```

Husk at kommandolinjeargumenter kun er tilgjengelige mens programmet kjører, så det kan være lurt å ha en logisk sjekk for å sikre at de er tilstede før du prøver å lese dem.

# Dypdykk
I dypdykk-seksjonen forklarer vi nærmere hvordan "argc" og "argv" variablene fungerer, samt noen nyttige funksjoner for å håndtere kommandolinjeargumenter.

"argc" variabelen (argument count) inneholder antall argumenter som ble gitt ved kjøring av programmet, inkludert navnet på selve programmet. For eksempel, hvis du kjører "program arg1 arg2", vil "argc" være 3.

"argv" variabelen (argument vector) er et array av strenger som inneholder alle argumentene som ble gitt ved kjøring av programmet. Navnet på programmet er alltid den første elementet på "argv" arrayet, etterfulgt av alle de andre argumentene.

I tillegg til å lese argumenter som vanlige strenger, kan du også bruke funksjoner som "atoi()" for å konvertere argumenter til andre datatyper, slik som heltall.

```C
// Konverterer første argument til heltall og lagrer det i en variabel
int tall = atoi(argv[1]);
```

Det finnes også funksjoner som "strcmp()" for å sammenligne argumenter med hverandre. Dette kan være nyttig hvis du for eksempel vil utføre ulike handlinger basert på hvilket argument som ble gitt.

```C
// Sjekker om første argument er lik "help"
if (strcmp(argv[1], "help") == 0) {
  // Utfør handlingen for å vise hjelpetekst
}
```

# Se også
- [Command Line Arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C Programming Tutorial - Command line arguments](https://www.learn-c.org/en/Command_Line_Arguments)