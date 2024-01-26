---
title:                "Sjekke om en mappe finnes"
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Å sjekke om en mappe eksisterer betyr å bekrefte at en spesifikk sti refereres til en reell katalog i filsystemet. Programmerere gjør dette for å unngå feil ved filoperasjoner eller for å beslutte om å opprette en ny mappe.

## How to:
I C++ kan du bruke filesystem biblioteket for å sjekke om en mappe eksisterer. Her er et eksempel:

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path dir = "/noen/mappe/sti";
    
    if (fs::exists(dir)) {
        std::cout << "Mappen eksisterer!" << std::endl;
    } else {
        std::cout << "Mappen finnes ikke." << std::endl;
    }
    
    return 0;
}
```

Dersom mappen eksisterer, vil du se:
```
Mappen eksisterer!
```

Hvis mappen ikke finnes, blir det:
```
Mappen finnes ikke.
```

## Deep Dive:
I eldre C++ kode brukes ofte `stat` fra `sys/stat.h` for å sjekke mappeer, men dette var plattformsavhengig og mer komplisert. Med C++17 introduserte `std::filesystem`, et kraftig og plattformuavhengig bibliotek, en effektiv og type-sikker måte å håndtere filsystem operasjoner. `fs::exists()` er en funksjon i dette biblioteket som sjekker om en fil eller mappe finnes. Til tross for enkelheten til `exists()`, er det viktig å bemerke at denne funksjonen kun indikerer eksistensen av en sti, ikke at det er en mappe, og derfor bør brukes i kombinasjon med `fs::is_directory()` om man trenger å være sikker.

## See Also:
- [std::filesystem dokumentasjon](https://en.cppreference.com/w/cpp/filesystem)
- [C++17 Nyheter](https://en.cppreference.com/w/cpp/17)
- [Boost.Filesystem hvis ikke C++17 er tilgjengelig](https://www.boost.org/doc/libs/release/libs/filesystem/)
