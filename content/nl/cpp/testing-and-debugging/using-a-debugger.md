---
title:                "Een debugger gebruiken"
aliases:
- /nl/cpp/using-a-debugger.md
date:                  2024-01-28T22:08:44.899315-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken betekent het opstarten van een hulpmiddel waarmee je binnenin je lopende programma kunt kijken om te begrijpen wat er echt gebeurt. Programmeurs doen dit om bugs te vinden en te verhelpen—die vervelende problemen die ervoor zorgen dat je code zich onverwacht gedraagt of crasht.

## Hoe:
C++ integreert met debuggers zoals GDB of de Visual Studio debugger. Hier is een hapklare voorbeeld met GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Oeps, deling door nul!
    std::cout << c << std::endl;
    return 0;
}

// Compileer met:
// g++ -g -o mijn_programma mijn_programma.cpp

// Uitvoeren met debugger:
// gdb ./mijn_programma
```

Zodra je GDB hebt gestart, kun je breakpoints instellen, stap voor stap door je code gaan, variabelen inspecteren en nog veel meer. Als je het bovenstaande uitvoert, zou je moeten zien dat je programma crasht vanwege de deling door nul.

## Diepe Duik
Debuggen vindt zijn oorsprong in de vroege dagen van programmeren, waar letterlijk het verwijderen van bugs (insecten!) uit hardware noodzakelijk was. Sindsdien zijn debughulpmiddelen geëvolueerd tot complexe en krachtige software, cruciaal voor ontwikkeling.

Alternatieven voor GDB voor C++ zijn onder andere LLDB, alsook IDE-geïntegreerde debuggers zoals die in Visual Studio, CLion, of Eclipse. Deze moderne omgevingen bieden grafische interfaces waardoor debuggen minder intimiderend wordt.

Implementatiedetails over het gebruik van een debugger hangen vaak af van je ontwikkelomgeving:

- Command-line debuggers (GDB, LLDB) vereisen bekendheid met terminalopdrachten en hebben vaak een steilere leercurve.
- Grafische debuggers vereenvoudigen het proces door interacties met point-and-click mogelijk te maken voor het instellen van breakpoints, stap-voor-stap door code te gaan en variabelen te bekijken.

Het begrijpen van de mogelijkheden van je debugger, zoals voorwaardelijke breakpoints, watchpoints, of het evalueren van expressies, kan je efficiëntie aanzienlijk verhogen bij het diagnosticeren van problemen.

## Zie Ook
- [GDB Documentatie](https://www.gnu.org/software/gdb/documentation/)
- [LLDB Commando Documentatie](https://lldb.llvm.org/use/map.html)
- [Visual Studio Debugger Tutorial](https://docs.microsoft.com/nl-nl/visualstudio/debugger/debugger-feature-tour)
- [Debuggen met CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
