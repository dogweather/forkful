---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Debug-Ausgabe drucken ist eine Methode zum Identifizieren und Lösen von Softwareproblemen. Programmierer nutzen es, um Live-Informationen zu erhalten und den Code-Fluss ihrer Programme zu verfolgen.

## Wie wird es gemacht:

Hier ist ein einfaches C++-Beispiel zum Drucken von Debug-Ausgabe. Wir können `std::cerr` verwenden, um in der Konsole zu drucken:

```C++
#include <iostream>
int main() {
    int variable = 5;
    std::cerr<<"Debug: Variable ist " << variable << '\n'; // Prints "Debug: Variable ist 5"
    return 0;
}
```

`std::cerr` ist für Fehlermeldungen gedacht, funktioniert aber auch perfekt für Debug-Ausgaben, da es immer auf der Konsole gedruckt wird, unabhängig von Umleitungen.

## Vertiefung

Historisch gesehen war die Debug-Ausgabe eine der ersten Methoden zur Fehlerbehebung in der Programmierung. Heute sind zwar fortschrittlichere Methoden wie Debugging und Profiling Frameworks verfügbar, der primäre Vorteil dieses Verfahrens liegt jedoch in seiner Einfachheit und Portabilität.

Ein alternatives Mittel zum Drucken von Debug-Ausgabe in C++ ist die Verwendung von Bibliotheken wie `boost.log` oder `spdlog`. Diese bieten mehr Kontrolle und Flexibilität aber erhöhen auch die Komplexität und die Anforderungen an den Code.

Der Hauptunterschied zwischen `std::cout` und `std::cerr` liegt in ihrer Pufferung: `std::cout` ist zeilenpufferiert, was bedeutet, dass der Inhalt erst dann ausgegeben wird, wenn der Puffer gefüllt ist oder ein Zeilenumbruch getroffen wird. `std::cerr` hingegen ist unmittelbar, es gibt den Text sofort aus.

## Siehe auch

1. [CPlusPlus-std::cerr](http://www.cplusplus.com/reference/iostream/cerr/)
2. [Boost Log Library](https://www.boost.org/doc/libs/1_76_0/libs/log/doc/html/index.html)
3. [Spdlog GitHub](https://github.com/gabime/spdlog)
4. [CPPReference-std::cerr](https://en.cppreference.com/w/cpp/io/cerr)