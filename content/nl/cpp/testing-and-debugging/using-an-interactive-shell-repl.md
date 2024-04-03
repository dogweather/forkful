---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:24.749032-07:00
description: "Een REPL (Read-Eval-Print-Loop) is een eenvoudige, interactieve programmeeromgeving.\
  \ Programmeurs gebruiken het voor realtime taalexperimenten, snelle\u2026"
lastmod: '2024-03-13T22:44:51.114247-06:00'
model: gpt-4-0125-preview
summary: Een REPL (Read-Eval-Print-Loop) is een eenvoudige, interactieve programmeeromgeving.
title: Het gebruik van een interactieve shell (REPL)
weight: 34
---

## Hoe te gebruiken:
C++ heeft geen ingebouwde REPL, maar tools zoals Cling bieden die mogelijkheid. Hier is hoe je Cling kunt gebruiken om de som van twee getallen te berekenen:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "De som is: " << a + b << std::endl;
    return 0;
}

// Uitvoer:
// De som is: 12
```

Start Cling en voer de code regel voor regel in, waarbij je de uitvoer na elke opdracht observeert. Het is directe feedback, zonder te compileren.

## Diepgaande duik
REPL's zijn gebruikelijk voor talen zoals Python of Lisp, en ze bestaan al sinds de jaren 60. Voor C++, een gecompileerde taal, past het concept niet zo natuurlijk, wat de reden is waarom tools zoals Cling bestaan - ze interpreteren C++ on-the-fly. Alternatieven omvatten online compilers of kleinschalige testprogramma’s die traditioneel worden gecompileerd. Cling is gebouwd op LLVM en Clang, en biedt een brug zodat C++ op een geïnterpreteerde manier gebruikt kan worden.

## Zie ook
- [Cling](https://root.cern/cling/): Een interactieve C++ interpreter, gebouwd op de top van LLVM en Clang bibliotheken.
- [Jupyter Notebooks](https://jupyter.org/): Biedt een interactieve shell binnen een notitieboekomgeving, ondersteunt C++ via de xeus-cling kernel.
- [LLVM](https://llvm.org/): Een verzameling van modulaire en herbruikbare compiler- en toolchain technologieën, waarop Cling is gebouwd.
