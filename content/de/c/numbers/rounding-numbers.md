---
title:                "Zahlen runden"
aliases: - /de/c/rounding-numbers.md
date:                  2024-02-03T18:07:21.364953-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/rounding-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Runden von Zahlen ist der Prozess der Anpassung der Ziffern einer Zahl, um deren Genauigkeit nach bestimmten Regeln zu verringern, entweder in Richtung der nächsten ganzen Zahl oder einer spezifizierten Anzahl von Dezimalstellen. Programmierer tun dies aus Gründen, die von der Begrenzung des benötigten Speicherplatzes bis hin zur Vereinfachung der Ausgabe für den Benutzer reichen, oder um genaue mathematische Operationen zu gewährleisten, die für sehr kleine Variationen empfindlich sind.

## Wie:

Das Runden von Zahlen in C kann mit verschiedenen Funktionen erreicht werden, der gebräuchlichste Ansatz beinhaltet jedoch die Funktionen `floor()`, `ceil()` und `round()`. Diese Funktionen sind Teil der Standard-Mathematikbibliothek, daher müssen Sie `math.h` in Ihrem Programm einbinden.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Verwenden von floor(), um abzurunden
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Verwenden von ceil(), um aufzurunden
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Verwenden von round(), um zur nächsten ganzen Zahl zu runden
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Runden auf eine spezifizierte Anzahl von Dezimalstellen beinhaltet Multiplikation und Division
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Runden auf zwei Dezimalstellen: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Ausgabe:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Runden auf zwei Dezimalstellen: 9.53
```

## Vertiefung

Das Runden von Zahlen hat tiefe historische Wurzeln in Mathematik und Berechnung, integral sowohl für theoretische als auch angewandte Aspekte. In C bieten zwar `floor()`, `ceil()` und `round()` grundlegende Funktionalität, aber das Wesen des Rundens von Fließkommazahlen zu Ganzzahlen oder spezifischen Dezimalstellen ist aufgrund der binären Darstellung von Fließkommazahlen nuancierter. Diese Darstellung kann zu unerwarteten Ergebnissen führen, da Zahlen, die nicht genau in Binär dargestellt werden können (wie 0.1), gehandhabt werden.

Diese Funktionen sind Teil der C-Standardbibliothek, definiert in `<math.h>`. Beim Runden von Zahlen, insbesondere für finanzielle oder präzise Ingenieurberechnungen, muss man die Implikationen der Verwendung binärer Fließkommazahlen berücksichtigen. Alternativen zu den eingebauten C-Funktionen für hochgenaues oder dezimalspezifisches Runden könnten das Implementieren benutzerdefinierter Rundungsfunktionen oder die Verwendung von Bibliotheken für die Berechnung mit beliebiger Präzision, wie GMP oder MPFR, umfassen, obwohl diese zusätzliche Komplexität und Abhängigkeiten einführen.

In der Praxis erfordert die Auswahl des richtigen Ansatzes zum Runden in C ein Gleichgewicht zwischen der Notwendigkeit von Präzision, Leistung und Praktikabilität, mit einem scharfen Verständnis für die domänenspezifischen Anforderungen der zu entwickelnden Anwendung.
