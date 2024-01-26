---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:40.449315-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen erzeugen in C bedeute, Werte zu generieren, die nicht vorhersagbar sind. Programmierer nutzen sie für alles Mögliche – von Spielentwicklung bis zu Simulationen.

## So geht's:
Generiere eine Zufallszahl zwischen 0 und 99 in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Initialisiere den Zufallszahlengenerator mit der aktuellen Zeit
    srand(time(NULL));

    // Generiere eine Zufallszahl zwischen 0 und 99
    int zufallszahl = rand() % 100;

    printf("Zufallszahl: %d\n", zufallszahl);
    return 0;
}
```

Beispiel-Ausgabe:
```
Zufallszahl: 42
```

## Deep Dive:
`rand()` ist eine Standardfunktion in C, die Zufallszahlen generiert. Allerdings, seit ihre Einführung in den 70er Jahren, wissen wir, dass `rand()` einige Einschränkungen hat – die erzeugten Zahlen sind nicht wirklich zufällig. Sie folgen einer vorbestimmten Sequenz, die mit `srand()` initialisiert wird. Für höhere Anforderungen an die Zufälligkeit gibt es bessere Methoden wie die Nutzung von `/dev/random` auf Unix-Systemen oder die Verwendung von speziellen Bibliotheken wie `pcg` oder `mtrand`.

Alternativen zu `rand()` und `srand()` wären Funktionen aus diesen Bibliotheken oder hardwarebasierte Zufallszahlengeneratoren, die echte Zufallswerte liefern.

Implementationsdetails: `rand()` verwendet typischerweise einen linearen Kongruenzgenerator (LCG), bei dem jedes neue Ergebnis von seinem Vorgänger abhängt. Das macht die Ergebnisse vorhersagbar, sobald der Startwert bekannt ist.

## Siehe auch:
- C Standard Library: `rand()` und `srand()` - http://en.cppreference.com/w/c/numeric/random/rand
- PCG, eine Familie von besserer Zufallszahlengeneratoren - http://www.pcg-random.org/
- mtrand, eine Implementierung des Mersenne Twister-Algorithmus - https://github.com/ESultanik/mtwister
