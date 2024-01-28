---
title:                "Zahlen runden"
date:                  2024-01-26T03:43:12.419869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zahlen runden bedeutet, einen Wert auf die nächste ganze Zahl oder eine angegebene Genauigkeit anzupassen. Entwickler tun dies, um zu vereinfachen, sich an reale Beschränkungen anzupassen oder die Leistung durch Verwerfen überschüssiger Genauigkeit zu verbessern.

## Wie geht das:
C++ bietet mehrere Möglichkeiten, Zahlen zu runden, wie `floor()`, `ceil()` und `round()`:

```C++
#include <iostream>
#include <cmath> // für Rundungsfunktionen

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Ausgabe: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Ausgabe: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Ausgabe: round: 3

    // Für feste Präzision, wie das Runden auf zwei Dezimalstellen:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "auf zwei Dezimalstellen gerundet: " << rounded << "\n"; // Ausgabe: auf zwei Dezimalstellen gerundet: 3.15

    return 0;
}
```

## Tiefergehend
Vor C++11 beruhte das Runden auf manuellen Techniken oder nicht standardisierten Bibliotheken. Heute stellt `<cmath>` robuste Methoden zur Verfügung. `floor()` rundet ab, `ceil()` rundet auf, während `round()` zur nächsten ganzen Zahl rundet, und sogar das Brechen von Unentschieden (0,5-Fälle) durch Runden zur geraden Zahl bewältigt.

Das Verständnis des Verhaltens dieser Funktionen ist entscheidend; beispielsweise könnten negative Zahlen Sie stolpern lassen (`std::round(-2.5)` ergibt `-2.0`).

Alternativen? Das Umwandeln in einen int nach dem Hinzufügen von 0.5 für positive Zahlen war ein klassischer Hack, aber mit negativen Zahlen fehleranfällig und nicht typagnostisch. Bibliotheken wie Boost können nuanciertere Ansätze bieten, während Spracherweiterungen oder Compiler-Intrinsics für spezifische Hardware optimieren können.

## Siehe auch
- C++ Referenz für `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE-Standard für Fließkommazahlen (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numeric Conversion Library: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
