---
title:    "C: Zufallszahlen generieren"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung. Es kann für verschiedene Anwendungen wie Spiele, Simulationen oder kryptografische Funktionen verwendet werden. Durch die Verwendung von Zufallszahlen können Programme dynamischer und weniger vorhersehbar gestaltet werden.

## Anleitung

Um Zufallszahlen in C zu generieren, können Sie die Standardbibliotheksfunktion ```rand()``` verwenden. Diese Funktion gibt eine Zufallszahl zwischen 0 und ```RAND_MAX``` zurück, die auf Ihrem System variieren kann. Um Zufallszahlen in einem bestimmten Bereich zu generieren, können Sie die Modulo-Operation verwenden. Zum Beispiel würde ```rand() % 10``` eine Zufallszahl zwischen 0 und 9 zurückgeben.

Hier ist ein Beispielcode, der 10 Zufallszahlen zwischen 1 und 100 generiert und sie in einer Schleife ausgibt:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Seed für die Zufallszahlengenerierung festlegen
    srand(42);

    // 10 Zufallszahlen zwischen 1 und 100 generieren und ausgeben
    for(int i = 0; i < 10; i++) {
        int zufallszahl = (rand() % 100) + 1; // Modulo 100 +1 für Bereich von 1 bis 100
        printf("%d. Zufallszahl: %d\n", i+1, zufallszahl);
    }
    
    return 0;
}
```

Das könnte folgende Ausgabe erzeugen:

```
1. Zufallszahl: 77
2. Zufallszahl: 92
3. Zufallszahl: 51
4. Zufallszahl: 14
5. Zufallszahl: 12
6. Zufallszahl: 87
7. Zufallszahl: 55
8. Zufallszahl: 35
9. Zufallszahl: 80
10. Zufallszahl: 23
```

## Tiefer Einblick

Die tatsächliche Implementierung von ```rand()``` in der Standardbibliothek kann je nach System unterschiedlich sein. In der Regel verwendet sie jedoch einen Pseudozufallszahlengenerator, der basierend auf einem Startwert (Seed) eine Sequenz von Zufallszahlen erzeugt. Standardmäßig wird der Seed auf ```1``` gesetzt, was bedeutet, dass bei jedem Programmlauf dieselbe Sequenz von Zufallszahlen erzeugt wird. Um unterschiedliche Ergebnisse zu erhalten, können Sie den Seed mithilfe der Funktion ```srand()``` auf einen anderen Wert setzen. Eine häufig verwendete Methode ist die Verwendung der Systemzeit, um den Seed zu setzen, da sich die Systemzeit bei jedem Programmlauf ändert.

Sie können auch Ihre eigenen Zufallszahlengeneratoren in C implementieren. Eine gängige Methode ist die Verwendung des Linear Congruential Generator, der die Formel ```X(n+1) = (a * X(n) + c) % m``` verwendet. Dabei ist ```a``` ein Multiplikator, ```c``` eine Konstante und ```m``` die Modulobergrenze. Die Wahl geeigneter Werte für diese Variablen ist wichtig, um eine gleichmäßige Verteilung der Zufallszahlen zu gewährleisten.

## Siehe auch

- [Generierung von Pseudozufallszahlen in C](https://de.wikipedia.org/wiki/Generierung_von_Pseudozufallszahlen_in_C)
- [C-Referenz: srand()](https://www.cplusplus.com/reference/cstdlib/srand/)
- [C-Referenz: rand()](https://www.cplusplus.com/reference/cstdlib/rand/)