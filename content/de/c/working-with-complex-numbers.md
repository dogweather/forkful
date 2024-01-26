---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:37:22.437026-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen, eine Mischung aus reellen und imaginären Teilen (wie 3 + 4i), sind entscheidend für fortgeschrittene Berechnungen, wie Signalverarbeitung oder das Lösen bestimmter Gleichungen. Programmierer verwenden sie für an Mathematik schwere Anwendungen, wo herkömmliche Zahlen nicht ausreichen.

## Wie:
C hat seit C99 einen nativen komplexen Typ und eine Bibliothek dafür. So verwendet man sie:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Deklariere zwei komplexe Zahlen
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operationen mit komplexen Zahlen
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Ausgabe der Ergebnisse
    printf("Summe: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Produkt: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Absolutwert & Phasenwinkel
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Beispielausgabe:
```
Summe: 3.0 + 1.0i
Produkt: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Vertiefung
Komplexe Zahlen reichen Jahrhunderte zurück, mit Wurzeln in der Algebra des 16. Jahrhunderts. Schnell vorwärts, sie sind jetzt ein Grundnahrungsmittel in vielen Programmiersprachen, nicht nur in C.

Der C99-Standard führte `<complex.h>`, einen Header ein, der Makros, Funktionen und den Datentyp `complex` definiert. Es gibt Alternativen - wie das Erstellen deiner eigenen Struktur, aber warum das Rad neu erfinden? Die C-Standardbibliothek ist optimiert und einsatzbereit.

Trotz seiner Stärke ist die Unterstützung von C für komplexe Zahlen nicht ohne Kritiker. Sie kann weniger intuitiv sein als ähnliche Funktionen in Sprachen wie Python, und das Behandeln von Spezialfällen kann knifflig werden. Aber für rohe Leistung ist sie immer noch eine solide Wahl.

## Siehe auch
- C99 Standarddokumentation für `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- IEEE-Standard für Gleitkommazahlen (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Online-Tutorial für die Mathematik komplexer Zahlen in C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming