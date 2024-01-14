---
title:                "C: Lesing av kommandolinje-argumenter"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan gjøre interaksjonen med et C-program mer fleksibelt og tilpassbart? Gjennom å lese kommandolinjeargumenter kan du gi brukerne dine muligheten til å påvirke programmet ditt på en enkel og effektiv måte. Les videre for å lære hvordan du kan implementere dette i ditt eget C-program.

## Hvordan

For å lese kommandolinjeargumenter i C-programmer, må du først inkludere <stdio.h> biblioteket i koden din. Deretter må du definere "int main(int argc, char *argv[])" for å kunne ta imot argumentene fra kommandolinjen i form av en array. Her er "argc" antall argumenter og "argv[]" er selve arrayen med argumentene.

For å få tilgang til de ulike argumentene, kan du bruke en "for"-løkke som går gjennom hele arrayen og skriver ut dem til terminalen. Et eksempel på dette kan se slik ut:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Når du kjører dette programmet og gir det noen argumenter fra kommandolinjen, vil det skrive ut argumentenes nummer og verdi til terminalen. For eksempel:

```
$ ./program.exe hello world
Argument 0: ./program.exe
Argument 1: hello
Argument 2: world
```

Du kan også bruke konverteringsfunksjoner som "atoi()" og "atof()" for å konvertere argumentene fra tekst til tall eller flyttall.

## Dypdykk

En annen nyttig funksjon for å lese kommandolinjeargumenter er "getopt()". Denne funksjonen lar deg håndtere kommandolinjen mer effektivt ved å tillate bruk av flagg og opsjoner. For å bruke denne funksjonen må du inkludere <unistd.h> biblioteket i koden din.

I tillegg til å håndtere argumentene, bør du også sjekke om brukeren har gitt riktig antall argumenter og gi en feilmelding hvis dette ikke stemmer. Dette kan gjøres ved å sammenligne "argc" med antall forventede argumenter.

## Se også

- [Kommunikasjon med kommandolinjen i C-programmer](https://www.ntnu.no/studier/emner/IMT3005/2017/hjemmesider/oblig4/lesfracli.html)
- [Getopt() dokumentasjon](https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html)
- ["atof()" konverteringsfunksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_atof.htm)