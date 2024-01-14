---
title:                "C++: Lesing av kommandolinjeargumenter"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Velkommen til en ny blogginnlegg om C++ programmering! Denne posten er spesielt for våre norske lesere som ønsker å lære mer om å lese kommandolinje-argumenter. Hvorfor ville noen engasjere seg i å lese kommandolinje-argumenter, spør du? Vel, det er flere grunner til dette. For det første, når du utvikler et program, vil du ofte måtte kommunisere med brukeren. Å lese kommandolinje-argumenter gir en enkel og direkte måte å gjøre dette på. Også, for å lage et program som er både fleksibelt og brukervennlig, er det avgjørende å kunne håndtere ulike inndata. Lesing av kommandolinje-argumenter tillater akkurat dette. Så la oss se på hvordan man gjør dette!

## Hvordan

For å lese kommandolinje-argumenter i C++, bruker vi funksjonen `main` og parametere `argc` og `argv`. La oss se på et eksempel:

```C++
#include <iostream>

int main(int argc, char *argv[])
{
    std::cout << "Antall argumenter: " << argc << std::endl;
    
    for(int i = 0; i < argc; i++)
    {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    
    return 0;
}
```
La oss si at vi lagrer denne koden i en fil kalt `argumenter.cpp`. Når vi kompilerer og kjører dette programmet i kommandolinjen, vil vi få følgende utskrift:

```
$ g++ argumenter.cpp -o argumenter
$ ./argumenter hei alle sammen
Antall argumenter: 4
Argument 0: ./argumenter
Argument 1: hei
Argument 2: alle
Argument 3: sammen
```

Her ser vi at `argc` parameteren inneholder antall argumenter (i dette tilfellet 4), og `argv` parameteren inneholder selve argumentene. Disse argumentene er delt opp og lagret som strenger i en array. Derfor kan vi bruke en `for`-løkke til å gå gjennom denne arrayen og skrive ut hvert argument.

## Dypdykk

Nå som vi har et grunnleggende eksempel, la oss se på noen flere detaljer når det gjelder å lese kommandolinje-argumenter i C++. Det første å merke seg er at `argc` parameteren alltid vil inneholde minst en verdi. Dette er fordi førsteelementet i `argv` alltid vil være navnet på programmet vårt (her `argumenter`). En annen ting å være oppmerksom på er at når vi leser argumenter i C++, vil alle argumentene bli lagret som strenger, selv om de faktisk er tall eller bokstaver. Så hvis vi ønsker å bruke dem som tall eller bokstaver, må vi konvertere dem.

En annen nyttig funksjon er `getopt()`, som er spesifikk for å lese og tolke kommandolinje-argumenter. Denne funksjonen lar oss spesifisere hvilke argumenter vi ønsker å lese, og deretter gir den oss tilgang til hvert individuelle argument.

## Se også

1. [Getopt i C++](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
2. [Guide til kommandolinjen for C++](https://www.cs.auckland.ac.nz/references/unix/digital/AQTLTBTE/DOCU_019.HTM)
3. [Lesing av standard input i C++](https://www.cplusplus.com/reference/iostream/istream/istream.html)