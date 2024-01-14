---
title:                "C++: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere kan føle at å lese kommandolinje-argumenter kan være en kjedelig og tidkrevende oppgave. Likevel, det er en viktig ferdighet som kan bidra til å effektivisere kode og gi mer fleksibilitet til brukere av programmet ditt. Derfor er det viktig å lære å lese kommandolinje-argumenter, og denne bloggposten vil vise deg hvordan.

## Hvordan man leser kommandolinje-argumenter i C++
Kommandolinje-argumenter kan leses enkelt gjennom å bruke et bibliotek kalt `getopt`. Dette biblioteket er tilgjengelig for C++ og kan hjelpe deg å hente ut argumentene som blir sendt til programmet ditt gjennom kommandolinjen.

For å bruke `getopt`, må du inkludere `<getopt.h>` i koden din. Her er et enkelt eksempel på hvordan du kan bruke `getopt` for å lese inn en enkelt kommandolinje-parameter og skrive den ut:

```C++
#include <iostream>
#include <getopt.h>

int main(int argc, char *argv[]) {
    // Initialiser variabler
    int c;
    int option;

    // Loop gjennom alle kommandolinje-argumenter
    while((c = getopt(argc, argv, "a:")) != -1) {
        switch(c) {
            case 'a':
                // Hent ut argumentet og skriv det ut
                option = atoi(optarg);
                std::cout << "Argumentet var: " << option << std::endl;
                break;
            default:
                // Hvis argumentet ikke stemmer overens med mulige alternativer
                std::cout << "Ukjent argument: " << c << std::endl;
        }
    }

    return 0;
}
```

La oss si at dette programmet heter "arguments" og du kjører det med argumentet `-a 42` i kommandolinjen. Da vil programmet skrive ut "Argumentet var: 42".

## Dypdykk i lesing av kommandolinje-argumenter
Så hva skjer egentlig i koden vår? Først og fremst så bruker vi `optarg` til å hente ut argumentet fra kommandolinjen. Dette er faktisk en peker til en streng som inneholder argumentet. Vi bruker også funksjonen `atoi()` for å konvertere argumentet til et tall.

I tillegg bruker vi `switch` og `case` for å håndtere ulike argumenter som kan bli sendt til programmet vårt. Hvis en bruker sender et argument som ikke stemmer overens med alternativene våre, vil programmet vårt gi en feilmelding.

Et annet viktig aspekt ved å lese kommandolinje-argumenter er at det gir oss mulighet til å gjøre programmet vårt mer fleksibelt. Vi kan for eksempel legge til flere alternativer i `switch` og `case` med ulike bokstaver, som vi kan bruke til å endre oppførselen til programmet vårt basert på brukerinput.

## Se også
- [getopt dokumentasjon](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [C++ kommandolinjen](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [Kommandolinje-argumenter i C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)

Takk for at du leste! Vi håper dette hjalp deg å forstå hvordan man kan lese kommandolinje-argumenter i C++. Lykke til med kodingen!