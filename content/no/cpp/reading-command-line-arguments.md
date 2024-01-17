---
title:                "Lese kommandolinje argumenter"
html_title:           "C++: Lese kommandolinje argumenter"
simple_title:         "Lese kommandolinje argumenter"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter er en måte for programvare å ta imot input fra terminalen mens den kjører. Dette kan være nyttig for å gi programmene våre mer fleksibilitet og mulighet til å håndtere forskjellige situasjoner. Programvareutviklere kan også bruke dette for å lage programmer som er enklere å bruke for brukere.

## Hvordan:
For å lese kommandolinjeargumenter i C++, kan du bruke variabelen "argc" (antall argumenter) og matrisen "argv" (argumenter). En enkel kode for å skrive ut alle argumentene kan se slik ut:

```
#include <iostream>

int main(int argc, char** argv) {
  for (int i = 0; i < argc; ++i) {
    std::cout << argv[i] << std::endl;
  }
  return 0;
}
```

Hvis vi for eksempel kjører dette programmet med kommandolinjen "./program navn år", vil det skrive ut:

```
./program
navn
år
```

## Dykk dypere:
Lesing av kommandolinjeargumenter har vært en del av operativsystemene siden Unix ble utviklet på 1970-tallet. Alternativene til å lese dem i C++ inkluderer å bruke "getopt" biblioteket eller å bruke en tredjepartsbibliotek som "Boost.Program_options". Implementasjonen av lesing av kommandolinjeargumenter er vanligvis avhengig av operativsystemet og språket som brukes.

## Se også:
- https://en.cppreference.com/w/cpp/utility/program/getenv
- https://www.gnu.org/software/libc/manual/html_node/Parsing-Program-Arguments.html