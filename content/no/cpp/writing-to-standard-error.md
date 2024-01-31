---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til standardfeil (`stderr`) handler om å sende feilmeldinger og diagnostikk uavhengig av vanlig output. Programmerere bruker dette for å skille vanlig dataflyt fra feilinformasjon, noe som er essensielt for feilsøking og loggføring.

## How to:
For å skrive til `stderr` i C++, bruk `std::cerr` eller `std::clog`. Veldig likt `std::cout`, men for feil og logger.

```C++
#include <iostream>

int main() {
    std::cerr << "Dette er en feilmelding til stderr." << std::endl;
    std::clog << "Dette er logginformasjon til stderr." << std::endl;
    return 0;
}
```

Forventing av output:
```
Dette er en feilmelding til stderr.
Dette er logginformasjon til stderr.
```

## Deep Dive:
`stderr` ble introdusert sammen med `stdin` og `stdout` i C for å håndtere I/O. Det har blitt beholdt i C++ for kompatibilitet og fordeling av ulike datastrømmer. Alternativt kan man bruke fillogging eller andre loggbiblioteker. Implementeringsmessig er `std::cerr` og `std::clog` globalt definert som instanser av `std::ostream` og er knyttet til standard error-strømmen av operativsystemet.

## See Also:
- C++ standard bibliotekdokumentasjon: https://en.cppreference.com/w/cpp/io/cerr
- Lær mer om I/O-strømmer i C++: https://www.learncpp.com/cpp-tutorial/basic-inputoutput-in-c/
- For en dypere forståelse av I/O i C og C++: http://www.cplusplus.com/reference/cstdio/
