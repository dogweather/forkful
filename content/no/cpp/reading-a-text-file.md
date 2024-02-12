---
title:                "Lese en tekstfil"
aliases:
- no/cpp/reading-a-text-file.md
date:                  2024-01-20T17:53:55.340757-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil i C++ handler om å få innholdet fra en fil på harddisken inn i programmet ditt. Vi gjør dette for å behandle data, lagre konfigurasjoner, eller laste inn ressurser.

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
