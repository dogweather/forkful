---
date: 2024-01-20 17:52:08.685338-07:00
description: "So geht's: Fr\xFCher nutzten Programmierer einfache `printf()`-Befehle\
  \ f\xFCr Debug-Meldungen. Mit der Einf\xFChrung von C++ Streams wurde `std::cout`\
  \ zum Standard\u2026"
lastmod: '2024-04-05T22:51:08.729469-06:00'
model: gpt-4-1106-preview
summary: "Fr\xFCher nutzten Programmierer einfache `printf()`-Befehle f\xFCr Debug-Meldungen."
title: Debug-Ausgaben drucken
weight: 33
---

## So geht's:
``` C++
#include <iostream>

int main() {
    // Variable zum Demonstrieren der Debug-Ausgabe
    int bedeutendeVariable = 42;

    // Debug-Ausgabe auf der Konsole
    std::cout << "Debug: bedeutendeVariable hat den Wert " << bedeutendeVariable << std::endl;

    // Weitere Logik hier…
    // ...

    return 0;
}
```
Erwartete Ausgabe:
```
Debug: bedeutendeVariable hat den Wert 42
```

## Deep Dive:
Früher nutzten Programmierer einfache `printf()`-Befehle für Debug-Meldungen. Mit der Einführung von C++ Streams wurde `std::cout` zum Standard für Ausgaben. Alternativ kann man `std::cerr` nutzen, um Fehlermeldungen auf dem Standardfehlerausgabe-Stream zu drucken. Die Implementierung ist einfach: Es reicht, geeignete Nachrichten an den gewünschten Stream zu senden. Allerdings sollte man diese Ausgaben vor der Veröffentlichung entfernen oder durch ein professionelles Logging-Framework ersetzen.

## Siehe Auch:
- CPP Reference zur I/O-Bibliothek: 
https://en.cppreference.com/w/cpp/header/iostream
- Guidelines für effektives Logging: 
https://www.vogella.com/tutorials/Logging/article.html
- Unterschied zwischen `std::cout` und `std::cerr`:
https://stackoverflow.com/questions/213907/c-stdendl-vs-n
