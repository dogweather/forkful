---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Utskrift av Debug Output i C++ 
*I denne artikkelen lærer du hvordan du kan skrive ut debug-informasjon i C++ programmene dine.*

## Hva & Hvorfor?
Utskrift av debug-output er et essensielt verktøy for programmerere brukes til diagnostisere og debugge kode. Det hjelper oss til å forstå hva som skjer i programmet ved kjøring ved å skrive ut ulike variabler og tilstander.

## Hvordan?
Her er et grunnleggende eksempel på hvordan du kan skrive ut debug-informasjon med C++:

```C++
#include<iostream>
#define NDEBUG
#include<assert.h>

int main() {
    int a = 5;
    std::cout << "Seksjon: 1, Variabel a = " << a << '\n';
    assert(a > 5 && "Error: a skal være mer enn 5");
    return 0;
}
```

Her er kjøreprosessen:

```bash
$ g++ -D NDEBUG -o main main.cpp
$ ./main
Seksjon: 1, Variabel a = 5
Assertion failed: (a > 5 && "Error: a skal være mer enn 5"), function main, file main.cpp, line 7.
```
## Dypdykk
Historisk sett, før kraftige IDEer som Visual Studio og JetBrains CLion, var utskrift av debug-output en hovedmetode for å debugge koden. Med introduksjonen av integrerte debuggere, har behovet minket, men det forblir en pålitelig og enkel metode for å se hva som skjer under kjøring.

Alternativene til utskrift av debug-output inkluderer bruk av en debugger som gjør det mulig å utføre trinnvise operasjoner, inspisere variabler og sette brytepunkt. 

Når det gjelder implementeringsdetaljer, bruker C++ `iostream` biblioteket for å skrive ut på konsollen, og bruk av `assert.h` biblioteket for å håndtere feil påstander under kjøring.

## Se Også
Til videre lesning og forståelse, sjekk ut følgende ressurser:

- C++ Standard Library: [link](http://www.cplusplus.com/reference/clibrary/)
- Debugging med GDB: [link](https://sourceware.org/gdb/current/onlinedocs/gdb/)
- Komplett guide for C++ Debugging: [link](https://www.jetbrains.com/help/clion/debugging-code.html)