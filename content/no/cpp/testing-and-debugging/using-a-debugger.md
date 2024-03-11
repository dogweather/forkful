---
date: 2024-01-26 03:48:08.619722-07:00
description: "\xC5 bruke en debugger betyr \xE5 starte et verkt\xF8y som lar deg kikke\
  \ inn i ditt kj\xF8rende program for \xE5 forst\xE5 hva som egentlig skjer. Programmere\
  \ gj\xF8r dette\u2026"
lastmod: '2024-03-11T00:14:14.697955-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en debugger betyr \xE5 starte et verkt\xF8y som lar deg kikke\
  \ inn i ditt kj\xF8rende program for \xE5 forst\xE5 hva som egentlig skjer. Programmere\
  \ gj\xF8r dette\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å bruke en debugger betyr å starte et verktøy som lar deg kikke inn i ditt kjørende program for å forstå hva som egentlig skjer. Programmere gjør dette for å finne og utrydde feil—de irriterende problemene som får koden din til å oppføre seg uventet eller krasje.

## Hvordan:
C++ integreres med debuggere som GDB eller Visual Studio-debuggeren. Her er et lite eksempel med bruk av GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Oops, divisjon med null!
    std::cout << c << std::endl;
    return 0;
}

// Kompiler med:
// g++ -g -o mitt_program mitt_program.cpp

// Kjør med debugger:
// gdb ./mitt_program
```

Når du har startet GDB, kan du sette brytepunkter, stegjennom kode, inspisere variabler og mye mer. Hvis du kjører det ovenstående, bør du se at programmet ditt krasjer på grunn av divisjon med null.

## Dyp dykk
Feilsøking har sine røtter i programmeringens tidlige dager, hvor det bokstavelig talt var nødvendig å fjerne insekter (bugs) fra maskinvaren. Siden da har verktøy for feilsøking utviklet seg til komplekse og kraftige programmer som er kritiske for utvikling.

Alternativer til GDB for C++ inkluderer LLDB, samt IDE-integrerte debuggere som de i Visual Studio, CLion eller Eclipse. Disse moderne miljøene tilbyr grafiske grensesnitt som gjør feilsøking mindre skremmende.

Implementeringsdetaljer om bruk av en debugger avhenger ofte av ditt utviklingsmiljø:

- Kommandolinjedebuggere (GDB, LLDB) krever kjennskap til terminalkommandoer og involverer ofte en brattere læringskurve.
- Grafiske debuggere forenkler prosessen ved å tillate pek-og-klikk-handlinger for å sette brytepunkter, stegjennom kode og overvåke variabler.

Å forstå debuggens kapasiteter, som betingede brytepunkter, vaktpunkter, eller evaluering av uttrykk, kan betydelig øke effektiviteten din i diagnostisering av problemer.

## Se også
- [GDB-dokumentasjon](https://www.gnu.org/software/gdb/documentation/)
- [LLDB Kommandodokumentasjon](https://lldb.llvm.org/use/map.html)
- [Visual Studio Debugger-opplæring](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [Feilsøking med CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
