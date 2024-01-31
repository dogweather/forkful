---
title:                "Zahlen runden"
date:                  2024-01-26T03:42:49.958859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Runden von Zahlen bedeutet, die Ziffern über einen bestimmten Punkt hinaus abzuschneiden, während optional die letzte behaltene Ziffer angepasst wird. Programmierer runden, um die Präzision zu reduzieren, wenn exakte Werte nicht notwendig sind, um Gleitkommazahlen-Fehler zu verwalten oder Zahlen für die benutzerfreundliche Anzeige vorzubereiten.

## Wie?
In C verwendet man typischerweise die Funktionen `floor()`, `ceil()` oder `round()`. Hier ist eine kurze Demonstration:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

Für mehr Kontrolle, wie zum Beispiel das Runden auf einen bestimmten Platz, multiplizieren Sie, runden und teilen:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Gerundet auf 2 Dezimalstellen: %.2f\n", num_rounded); // Gerundet auf 2 Dezimalstellen: 3.14
```

## Tiefgreifender Einblick
Früher bedeutete Runden oft einen manuellen Prozess – eine schwere Aufgabe nur mit Stift und Papier. Mit der Computertechnik haben wir dies automatisiert, aber die Gleitkommaarithmetik brachte Nuancen mit sich aufgrund ihrer binären Natur, wo einige Zahlen nicht exakt dargestellt werden können.

Alternativen zum Standardrunden beinhalten das Trunkieren (einfaches Fallenlassen zusätzlicher Ziffern) oder das Bankiersrunden, das zum nächsten geraden Zahlenwert rundet, wenn genau zwischen zwei Werten, um den Bias in wiederholten Berechnungen zu reduzieren.

Die Implementierung wird knifflig, wenn Sie beliebig präzise Zahlen runden müssen oder spezielle Fälle wie Unendlichkeit, signalisierende NaNs oder subnormale Werte behandeln müssen. Die Funktionen der C-Standardbibliothek bewältigen die Grundlagen, aber wenn Sie Dezimalzahlen auf individuelle Weise runden müssen, benötigen Sie mehr als `math.h`.

## Siehe auch
- [`<math.h>` Dokumentation](https://en.cppreference.com/w/c/numeric/math)
- [Gleitkommaarithmetik](https://de.wikipedia.org/wiki/Gleitkommazahl)
- [Die Fallstricke der Überprüfung von Gleitkommaberechnungen](https://dl.acm.org/doi/10.1145/1186736.1186737)
