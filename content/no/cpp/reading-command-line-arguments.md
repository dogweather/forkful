---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "C++: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lese kommandolinjeargumenter er en nyttig ferdighet for enhver C++ programmerer. Det lar deg hente informasjon fra brukeren på en enkel og effektiv måte, noe som er viktig for å lage interaktive programmer.

## Hvordan
For å lese kommandolinjeargumenter i C++, må du bruke funksjonen `int main(int argc, char* argv[])`. `argc` representerer antall argumenter som er gitt av brukeren, mens `argv` er en matrise av strenger som inneholder disse argumentene.

Her er et eksempel på hvordan du kan bruke denne funksjonen for å lese et enkelt argument fra brukeren og skrive det ut:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    if (argc != 2) { // Sjekker om det er nøyaktig ett argument gitt
        std::cout << "Bruk: ./programnavn <argument>" << "\n";
        return 1; // Returnerer en feilkode
    }
    
    std::cout << "Argumentet du ga var: " << argv[1] << "\n";
    return 0; // Programmet avsluttes uten feil
}
```

Hvis du kjører dette programmet med `./programnavn hei`, vil det skrive ut "Argumentet du ga var: hei".

Du kan også bruke en løkke til å lese gjennom alle argumentene gitt og utføre forskjellige handlinger basert på dem.

## Dypdykk
I tillegg til `argc` og `argv`, har du tilgang til andre nyttige funksjoner for å jobbe med kommandolinjeargumenter. En av dem er `std::string_view`, som lar deg enkelt jobbe med argumentene som strenger.

Du kan også bruke biblioteket `getopt` for å behandle argumentene mer strukturert. Det lar deg definere hvilke argumenter som er tillatt, og håndtere forskjellige kombinasjoner av flagg og verdier.

Det er viktig å merke seg at rekkefølgen på argumentene som gis av brukeren er viktig. For eksempel, hvis du har et program som tar inn en filsti og en flagg, må du sørge for at filstien er spesifisert før flagget.

## Se også
- [C++ Dokumentasjon om kommandolinjeargumenter](https://en.cppreference.com/w/cpp/language/main_function)
- [GitHub side for `getopt` biblioteket](https://github.com/kimwalisch/getopt)