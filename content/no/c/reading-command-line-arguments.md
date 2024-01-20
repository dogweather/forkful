---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese kommandolinjeargumenter er prosessen med å hente data gitt til ditt C-program via kommandolinjen. Det er en typisk måte for programmererne å parametere og konfigurere programmer på.

## Hvordan gjøre det:
Her er et grunnleggende eksempel på hvordan du leser kommandolinjeargumentene i C:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int tel = 0;
    printf("Program navn er %s\n", argv[0]);
    for(tel = 1; tel < argc; tel++) {
       printf("Argument nummer %d er %s\n", tel, argv[tel]);
    }
    return 0;
}
```
Når du kjører programmet med argumenter (for eksempel `./programmet ditt Hei alle sammen`), vil output være:

```
Program navn er ./programmet ditt
Argument nummer 1 er Hei
Argument nummer 2 er alle
Argument nummer 3 er sammen
```

## Dyp Dykk
Historisk sett har kommandolinjeargumenter blitt brukt siden de tidligste dagene av programmering, spesielt i Unix- og Linux-baserte systemer, for å manipulere programmet på kjøretid.

Alternativene inkluderer bruk av filer for inn- og utdata, interaktiv brukerinput og data hentet fra nettverket. Kommandolinjeargumenter kan være en effektiv måte å sende informasjon til et program, men de er ikke alltid den mest praktiske eller sikreste metoden.

Den grunnleggende implementeringen av kommandolinjeargumenter i C er gjennom bruk av to funksjonsparametere i `main()`: `int argc` og `char *argv[]`. `argc` teller antall argumenter, og `argv` er en peker til peker-array som holder argumentene selv.

## Se også
For mer detaljert informasjon, sjekk ut disse nyttige lenkene:
1. [Command Line Arguments in C](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
3. [Command Line Arguments in Unix](https://www.ibm.com/docs/en/aix/7.2?topic=applications-command-line-arguments)