---
date: 2024-01-20 17:53:55.340757-07:00
description: "Slik gj\xF8r du: \xC5 lese filer i C++ g\xE5r langt tilbake og har endret\
  \ seg lite med \xE5rene. Alternativer til `std::ifstream` inkluderer `FILE` fra\
  \ C\u2026"
lastmod: '2024-04-05T22:50:55.124772-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese filer i C++ g\xE5r langt tilbake og har endret seg lite med \xE5\
  rene."
title: Lese en tekstfil
weight: 22
---

## Slik gjør du:
```C++
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream file("eksempel.txt");
    if (file.is_open()) {
        std::string line;
        while (getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Kunne ikke åpne filen." << std::endl;
    }
    return 0;
}
```
**Utdataeksempel:**
```
Første linje i tekstfilen
Andre linje i tekstfilen
Tredje linje i tekstfilen
```

## Dypdykk
Å lese filer i C++ går langt tilbake og har endret seg lite med årene. Alternativer til `std::ifstream` inkluderer `FILE` fra C standardbiblioteket og biblioteker som Boost. Implementasjonsdetaljer inkluderer håndtering av ulike filmoduser (som binær eller tekst), og feilsøking med medlemsfunksjonen `std::ios::fail()`.

## Se også
- C++ fil IO-dokumentasjon: https://en.cppreference.com/w/cpp/io
- Ressurser for å lære om Boost-biblioteket: https://www.boost.org/
- Utforsk `std::filesystem` for moderne filsystemoperasjoner: https://en.cppreference.com/w/cpp/filesystem
