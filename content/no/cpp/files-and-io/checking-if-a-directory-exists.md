---
aliases:
- /no/cpp/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:56.406676-07:00
description: "\xC5 sjekke om en mappe eksisterer handler om \xE5 fastsl\xE5 tilstedev\xE6\
  relsen av en mappe p\xE5 en angitt sti f\xF8r man utf\xF8rer operasjoner som \xE5\
  \ lese fra eller skrive\u2026"
lastmod: 2024-02-18 23:08:54.241361
model: gpt-4-0125-preview
summary: "\xC5 sjekke om en mappe eksisterer handler om \xE5 fastsl\xE5 tilstedev\xE6\
  relsen av en mappe p\xE5 en angitt sti f\xF8r man utf\xF8rer operasjoner som \xE5\
  \ lese fra eller skrive\u2026"
title: Sjekker om en mappe eksisterer
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å sjekke om en mappe eksisterer handler om å fastslå tilstedeværelsen av en mappe på en angitt sti før man utfører operasjoner som å lese fra eller skrive til filer inni den. Programmerere gjør dette for å unngå feil relatert til filoperasjoner, og sørger for en jevnere og mer pålitelig utføring av oppgaver knyttet til filhåndtering i applikasjonene deres.

## Hvordan:
I moderne C++ (C++17 og videre) kan du bruke filsystembiblioteket til å sjekke om en mappe eksisterer. Det gir en enkel og standardisert måte å utføre filsystemoperasjoner på, inkludert å sjekke for eksistensen av en mappe.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/sti/til/mappe";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Mappen eksisterer." << std::endl;
    } else {
        std::cout << "Mappen eksisterer ikke." << std::endl;
    }

    return 0;
}
```
Eksempel på utskrift hvis mappen eksisterer:
```
Mappen eksisterer.
```

Eksempel på utskrift hvis mappen ikke eksisterer:
```
Mappen eksisterer ikke.
```

For prosjekter som ennå ikke bruker C++17 eller for ekstra funksjonalitet, er Boost Filesystem-biblioteket et populært tredjepartsvalg som tilbyr lignende funksjonalitet.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/sti/til/mappe";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Mappen eksisterer." << std::endl;
    } else {
        std::cout << "Mappen eksisterer ikke." << std::endl;
    }

    return 0;
}
```
Ved å bruke Boost Filesystem vil utskriften være identisk med eksempelet fra C++17 filsystemet, avhengig av eksistensen av mappen på den angitte stien.
