---
date: 2024-01-20 17:55:29.219355-07:00
description: "\xC5 lese kommandolinjeargumenter betyr \xE5 fange opp data gitt direkte\
  \ n\xE5r du kj\xF8rer programmet ditt. Programmere gj\xF8r dette for \xE5 tillate\
  \ dynamiske\u2026"
lastmod: '2024-03-13T22:44:41.115521-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese kommandolinjeargumenter betyr \xE5 fange opp data gitt direkte\
  \ n\xE5r du kj\xF8rer programmet ditt."
title: Lese kommandolinjeargumenter
weight: 23
---

## How to (Slik gjør du det)
```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "Det er " << argc << " argument(er):\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }
    return 0;
}

```
Kjører du `./programmet ditt klare fjellvann`, får du:
```
Det er 4 argument(er):
0: ./programmet ditt
1: klare
2: fjellvann
```

## Deep Dive (Dypdykk)
Før 1980-tallet brukte programmerere kommandolinjen for nesten all interaksjon med datamaskiner. Å lese kommandolinjeargumenter er en gammel tradisjon som står fast fordi det er effektivt. Alternativene inkluderer interaktiv input og konfigurasjonsfiler, men de er tregere for enkelte oppgaver. De tekniske detaljene involverer funksjonen `main` som får to parametere: `argc` (argument count) som forteller hvor mange argumenter, og `argv` (argument vector), som er et array av C-strenger (char pointers).

## See Also (Se også)
- [cppreference.com - Main function](https://en.cppreference.com/w/cpp/language/main_function)
- [GNU: Using the GNU Compiler Collection (GCC) - Environment Variables](https://gcc.gnu.org/onlinedocs/gcc/Environment-Variables.html)
- [Stack Overflow - How can I get the list of files in a directory using C or C++?](https://stackoverflow.com/questions/612097/how-can-i-get-the-list-of-files-in-a-directory-using-c-or-c)
