---
title:                "Refaktorisierung"
aliases: - /de/c/refactoring.md
date:                  2024-02-03T18:06:45.745526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisierung"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/refactoring.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring in der Programmierung beinhaltet die Umstrukturierung bestehenden Codes, ohne dessen externes Verhalten zu ändern, mit dem Ziel, nicht-funktionale Attribute wie Lesbarkeit zu verbessern, Komplexität zu reduzieren und die Wartbarkeit zu erhöhen. Programmierer führen ein Refactoring durch, um die Codebasis sauber zu halten, technische Schulden zu minimieren und zukünftige Änderungen einfacher und sicherer zu implementieren.

## Wie:

Refactoring kann eine Reihe von Taktiken beinhalten, von der Umbenennung von Variablen zur Klarheit bis hin zur Änderung der Struktur des Codes für eine bessere Modularisierung. Hier ist ein einfaches Beispiel, das zeigt, wie man ein Stück C-Code für bessere Klarheit und Effizienz refaktorisieren kann.

Vor dem Refactoring:
```c
#include <stdio.h>

int main() {
    int x = 10, y = 20;
    printf("Vor dem Tauschen: x = %d, y = %d\n", x, y);
    x = x + y; // x ist jetzt 30
    y = x - y; // y wird 10
    x = x - y; // x wird 20
    printf("Nach dem Tauschen: x = %d, y = %d\n", x, y);
    return 0;
}
```
Ausgabe:
```
Vor dem Tauschen: x = 10, y = 20
Nach dem Tauschen: x = 20, y = 10
```
Nach dem Refactoring:
```c
#include <stdio.h>

void swap(int *a, int *b) {
    *a = *a + *b;
    *b = *a - *b;
    *a = *a - *b;
}

int main() {
    int x = 10, y = 20;
    printf("Vor dem Tauschen: x = %d, y = %d\n", x, y);
    swap(&x, &y);
    printf("Nach dem Tauschen: x = %d, y = %d\n", x, y);
    return 0;
}
```
Die Ausgabe bleibt unverändert, aber die Funktionalität zum Tauschen von Werten wurde in eine separate Funktion (`swap`) verschoben, was die Lesbarkeit und Wiederverwendbarkeit verbessert.

## Tiefergehend

Die Praxis des Refactoring von Codes existiert so lange wie die Softwareentwicklung selbst und hat sich parallel zu Programmierparadigmen und -sprachen entwickelt. In C, einer Sprache, die sowohl mächtig als auch voller Möglichkeiten für Ineffizienz und Fehler aufgrund ihrer Low-Level-Natur ist, ist Refactoring besonders wichtig. Es kann den Unterschied ausmachen zwischen einer wartbaren Codebasis und einem verworrenen Netz von Ineffizienzen.

Eine Überlegung, die speziell für C gilt, ist das Gleichgewicht zwischen Mikro-Optimierungen und Lesbarkeit/Wartbarkeit. Obwohl es verlockend ist, C-Code für jedes bisschen Leistung manuell zu optimieren, können solche Optimierungen den Code brüchiger und schwerer lesbar machen. Daher ist es in der Regel besser, sauberen, lesbaren Code zu priorisieren und sich auf den Optimierer des Compilers zu verlassen, um Leistungsverbesserungen zu ermöglichen, wo es möglich ist.

Darüber hinaus haben Werkzeuge und Techniken für das Refactoring in C, wie statische Code-Analysatoren (z.B. Clang Static Analyzer, cppcheck) und Prinzipien der modularen Programmierung, erheblich Fortschritte gemacht. Aufgrund des manuellen Speichermanagements und der Zeigerarithmetik von C kann Refactoring jedoch Fehler einführen, wenn es nicht sorgfältig durchgeführt wird. Techniken wie Unit Testing und Code-Review sind hier von unschätzbarem Wert.

Während neuere Sprachen mehr eingebaute Unterstützung für sicheres Refactoring mit Funktionen wie automatischer Speicherverwaltung und reichen Typsystemen bieten, bleibt C in Szenarien unübertroffen, die Leistung nahe der Maschine und feinkörnige Kontrolle erfordern. In solchen Fällen geht es beim Refactoring weniger darum, Sprachfunktionen zu nutzen, und mehr um disziplinierte, durchdachte Umstrukturierung von Code.
