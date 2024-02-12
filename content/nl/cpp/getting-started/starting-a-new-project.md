---
title:                "Een nieuw project starten"
aliases: - /nl/cpp/starting-a-new-project.md
date:                  2024-01-28T22:08:13.889275-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw project starten betekent het opzetten van de basis voor je codebase. Programmeurs doen dit om het ontwikkelingsproces op gang te brengen, de structuur van het project te leiden en de basis te leggen voor toekomstige code.

## Hoe:
Bij het beginnen, kies je bouwsysteem of IDE. Voor eenvoud gebruiken we een eenvoudige teksteditor en g++. Maak twee bestanden: `main.cpp` en een `Makefile`.

`main.cpp`:
```C++
#include <iostream>

int main() {
    std::cout << "Hallo, nieuw project!" << std::endl;
    return 0;
}
```

`Makefile`:
```make
all:
    g++ main.cpp -o mijn_project

clean:
    rm mijn_project
```

Om te compileren, voer `make` uit in de terminal. Om op te ruimen, voer `make clean` uit.

Voorbeelduitvoer na het uitvoeren van `./mijn_project`:
```
Hallo, nieuw project!
```

## Diepgaand
Historisch gezien was het opzetten van een nieuw C++ project een meer handmatig proces. Tegenwoordig kunnen IDE's sjablonen genereren. Keuzes als CMake of Meson helpen bij het beheren van builds. Voordat deze tools bestonden, schreven ontwikkelaars Makefiles met de hand, waarbij elk `.cpp` bestand naar een objectbestand werd gecompileerd voordat ze werden gelinkt.

Alternatieven overwegende: nieuwere bouwsystemen vereenvoudigen het proces. Bijvoorbeeld, CMake genereert automatisch je Makefiles, waardoor het platformonafhankelijk wordt.

Wat de implementatie betreft, hangt de opzet af van factoren zoals projectgrootte en afhankelijkheden. Grotere projecten vereisen een complexere structuur met aparte mappen voor bronbestanden, headers en tests.

## Zie Ook
- [CMake Documentatie](https://cmake.org/documentation/)
- [C++ Kernrichtlijnen](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [GCC, de GNU Compiler Collectie](https://gcc.gnu.org/)
