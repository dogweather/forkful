---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:53.913110-07:00
description: "Wie: In C werden komplexe Zahlen durch die Standardbibliothek unterst\xFC\
  tzt, insbesondere durch `<complex.h>`. Um sie zu nutzen, deklarieren Sie Variablen\u2026"
lastmod: '2024-03-13T22:44:54.345635-06:00'
model: gpt-4-0125-preview
summary: "In C werden komplexe Zahlen durch die Standardbibliothek unterst\xFCtzt,\
  \ insbesondere durch `<complex.h>`."
title: Arbeiten mit komplexen Zahlen
weight: 14
---

## Wie:
In C werden komplexe Zahlen durch die Standardbibliothek unterstützt, insbesondere durch `<complex.h>`. Um sie zu nutzen, deklarieren Sie Variablen mit dem Typ `double complex` (oder `float complex` für einfache Genauigkeit). Hier ist, wie man grundlegende Operationen durchführt:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Deklarieren einer komplexen Zahl 1+2i
    double complex z2 = 1.0 - 2.0*I; // Deklarieren einer weiteren komplexen Zahl 1-2i
    
    // Addition
    double complex sum = z1 + z2;
    printf("Summe: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Ausgabe: Summe: 2.00 + 0.00i

    // Multiplikation
    double complex product = z1 * z2;
    printf("Produkt: %.2f + %.2fi\n", creal(product), cimag(product)); // Ausgabe: Produkt: 5.00 + 0.00i

    // Komplex Konjugiert
    double complex conjugate = conj(z1);
    printf("Konjugiert von z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Ausgabe: Konjugiert von z1: 1.00 - 2.00i
    
    // Betrag
    double magnitude = cabs(z1);
    printf("Betrag von z1: %.2f\n", magnitude); // Ausgabe: Betrag von z1: 2.24

    // Phase
    double phase = carg(z1);
    printf("Phase von z1: %.2f\n", phase); // Ausgabe in Radianten
    
    return 0;
}
```
Beachten Sie, dass `I` eine Konstante ist, die die imaginäre Einheit in `<complex.h>` darstellt. Funktionen wie `creal()` und `cimag()` extrahieren den Real- bzw. Imaginärteil, während `conj()` das komplexe Konjugat berechnet. Für den Betrag und die Phase (Argument) von komplexen Zahlen werden `cabs()` und `carg()` verwendet.

## Tiefergehend
Die Unterstützung für komplexe Zahlen in C ist relativ neu und wurde in C99 standardisiert. Vor dieser Zeit war die Arithmetik mit komplexen Zahlen in C umständlich, was oft eigene Datenstrukturen und Funktionen erforderte. Die Aufnahme von `<complex.h>` und die komplexen Datentypen stellten einen signifikanten Schub für die Fähigkeiten der Sprache für wissenschaftliche und ingenieurtechnische Anwendungen dar. Es ist jedoch zu beachten, dass einige Sprachen, wie Python, intuitivere Unterstützung für komplexe Zahlen durch eingebaute Datentypen und ein reichhaltigeres Set an Bibliotheksfunktionen bieten. Trotzdem bleibt C aufgrund der Leistung und Kontrolle, die es bietet, eine bevorzugte Wahl für Aufgaben mit hoher Rechenleistung, auch wenn dies bedeutet, sich mit einer etwas umständlicheren Syntax für komplexe Arithmetik auseinanderzusetzen.
