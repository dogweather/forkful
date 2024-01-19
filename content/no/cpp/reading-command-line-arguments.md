---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinjeargumenter er informasjon som direkte matet inn i programmer når de kjører. Programmers leser disse for å tilpasse programutførelsen basert på brukerens inngang.

## Hvordan?

Her er en enkel kode for å lese kommandolinjeargumenter i C++. 

```C++
#include <iostream>

int main(int argc, char** argv) {
    for(int i=0; i< argc; i++) {
        std::cout << "Arg " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Når du kjører dette programmet med argumenter, vil du få følgende utgang:

``` 
$ ./program arg1 arg2 arg3
Arg 0: ./program
Arg 1: arg1
Arg 2: arg2
Arg 3: arg3
```

## Dyp Dykk

Historisk kontekst: Lesing av kommandolinjeargumenter har sitt opphav fra Unix-systemer der kommandolinjeverktøyer krever denne funksjonaliteten for fleksibilitets- og brukertilpasningsegenskaper.

Alternativer: Boost.Program_options og TCLAP er eksempler på biblioteker som kan forenkle arbeidet med å håndtere kommandolinjeargumenter.

Implementeringsdetaljer: Argumentene som er gitt til et program, blir lagret i en tabell av strenger (argv) som sendes til main() -funksjonen. Antallet argumenter (argc) blir også sendt.

## Se Også

Nedenfor er linker til noen nyttige ressurser om C++ kommandolinjeargumenter:

- C++ Standard Library: http://www.cplusplus.com/reference/cstdlib/
- Boost.Program_options: https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html
- TCLAP - Templatized C++ Command Line Parser: http://tclap.sourceforge.net/