---
date: 2024-01-26 04:12:16.280405-07:00
description: "Hvordan: C++ kommer ikke med innebygd REPL, men verkt\xF8y som Cling\
  \ tilbyr denne muligheten. Her er hvordan du bruker Cling for \xE5 beregne summen\
  \ av to tall."
lastmod: '2024-03-13T22:44:41.101550-06:00'
model: gpt-4-0125-preview
summary: "C++ kommer ikke med innebygd REPL, men verkt\xF8y som Cling tilbyr denne\
  \ muligheten."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
C++ kommer ikke med innebygd REPL, men verktøy som Cling tilbyr denne muligheten. Her er hvordan du bruker Cling for å beregne summen av to tall:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "Summen er: " << a + b << std::endl;
    return 0;
}

// Utdata:
// Summen er: 12
```

Start Cling og skriv inn koden linje for linje, og observer utdataene etter hver kommando. Det er umiddelbar tilbakemelding, uten å kompilere.

## Dypdykk
REPLs er vanlige for språk som Python eller Lisp, og de har vært rundt siden 1960-tallet. For C++, et kompilert språk, passer ikke konseptet like naturlig, derfor eksisterer verktøy som Cling - de tolker C++ på stedet. Alternativer inkluderer online kompilatorer eller småskala testprogrammer kompilert på tradisjonelt vis. Cling er bygget på toppen av LLVM og Clang, og tilbyr en bro for at C++ kan brukes på en tolket måte.

## Se også
- [Cling](https://root.cern/cling/): En interaktiv C++ tolker, bygget på toppen av LLVM- og Clang-biblioteker.
- [Jupyter Notebooks](https://jupyter.org/): Tilbyr et interaktivt skall i et notatbokmiljø, støtter C++ gjennom xeus-cling kjernen.
- [LLVM](https://llvm.org/): En samling av modulære og gjenbrukbare kompilator- og verktøykjedeteknologier, som Cling bygger på.
