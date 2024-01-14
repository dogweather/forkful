---
title:    "C++: Å lese kommandolinje-argumenter"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Lesing av kommandolinjeargumenter er en veldig nyttig ferdighet som kan hjelpe deg å lage mer interaktive og tilpasningsdyktige programmer. Det er også en viktig del av å forstå hvordan datamaskiner fungerer, og kan være nyttig i løpet av din programmeringskarriere.

## Hvordan

For å lese kommandolinjeargumenter i C++, må du bruke `main` funksjonen og inkludere `argc` og `argv` som parametere. `argc` representerer antall argumenter som kommer etter programnavnet, mens `argv` er en matrise som inneholder selve argumentene. La oss se på et eksempel:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // Skriv ut antall argumenter
    std::cout << "Antall kommandolinjeargumenter: " << argc << std::endl;

    // Gå gjennom argumentene og skriv dem ut
    for (int i = 0; i < argc; i++) {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    
    return 0;
}
```

Kompilering og kjøring av dette programmet vil gi følgende output:

```
$ ./programnavn 123 hello world
Antall kommandolinjeargumenter: 4
Argument 0: ./programnavn
Argument 1: 123
Argument 2: hello
Argument 3: world
```

Du kan også bruke `std::string` til å håndtere argumentene på en mer fleksibel måte. For eksempel:

```C++
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    // Sjekk om det er nok argumenter
    if (argc < 2) {
        std::cout << "Vennligst angi navnet ditt som argument." << std::endl;
        return 1;
    }

    // Bruk det første argumentet som navn
    std::string navn = argv[1];

    // Skriv ut en personlig hilsen
    std::cout << "Hei " << navn << ", velkommen til programmet!" << std::endl;

    return 0;
}
```

Her sjekker vi om det er nok argumenter, og hvis ikke, gir vi en feilmelding og avslutter programmet. Hvis det er nok argumenter, bruker vi det første argumentet som navn og skriver ut en personlig hilsen. Kompiler og kjør programmet, og du vil kunne skrive inn navnet ditt som argument og få en personlig hilsen.

## Dykk dypere

Nå som du vet hvordan du leser kommandolinjeargumenter i C++, kan du også lære å analysere argumentene og utføre ulike handlinger basert på dem. Du kan også bruke `getopt()` funksjonen for å behandle mer komplekse argumenter, som for eksempel flagg og verdier.

Se også:

- [https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [https://www.learncpp.com/cpp-tutorial/command-line-arguments/](https://www.learncpp.com/cpp-tutorial/command-line-arguments/)
- [https://en.cppreference.com/w/cpp/utility/getopt](https://en.cppreference.com/w/cpp/utility/getopt)