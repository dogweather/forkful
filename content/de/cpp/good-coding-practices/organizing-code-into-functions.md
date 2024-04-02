---
date: 2024-01-26 01:09:29.131365-07:00
description: "Code in Funktionen zu unterteilen, bedeutet, den Code in kleinere, wiederverwendbare\
  \ St\xFCcke zu zerlegen. Wir tun dies, um Wiederholungen zu vermeiden,\u2026"
lastmod: '2024-03-13T22:44:54.190224-06:00'
model: gpt-4-1106-preview
summary: "Code in Funktionen zu unterteilen, bedeutet, den Code in kleinere, wiederverwendbare\
  \ St\xFCcke zu zerlegen. Wir tun dies, um Wiederholungen zu vermeiden,\u2026"
title: Code in Funktionen organisieren
weight: 18
---

## Was & Warum?
Code in Funktionen zu unterteilen, bedeutet, den Code in kleinere, wiederverwendbare Stücke zu zerlegen. Wir tun dies, um Wiederholungen zu vermeiden, unseren Code lesbar zu machen und das Debugging und Testen zu vereinfachen. Gut organisierte Funktionen können wie eine Kiste mit ordentlich beschrifteten Werkzeugen sein, bereit zum Gebrauch und zum Teilen.

## Wie geht das:
Nehmen wir eine alltägliche Aufgabe: die Berechnung der Fläche eines Kreises. Anstatt jedes Mal die gleiche Formel zu schreiben, kapseln wir sie in eine Funktion ein.

```C++
#include <iostream>
#define PI 3.14159

double berechneKreisflaeche(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Fläche des Kreises mit Radius " << r << " beträgt " << berechneKreisflaeche(r) << std::endl;
    return 0;
}
```

Beispielausgabe:
```
Fläche des Kreises mit Radius 5 beträgt 78.5397
```

## Vertiefung
Historisch gesehen waren Prozeduren und Funktionen das Rückgrat der strukturierten Programmierung, die in den 1960er Jahren gefördert wurde, um die Probleme des "Spaghetticodes" in früheren imperativen Programmiersprachen zu bekämpfen. Alternativen wie OOP (Objektorientierte Programmierung) gehen noch weiter, indem sie diese Funktionen mit Datenstrukturen verknüpfen. In C++ gibt es reguläre Funktionen, Klassenmethoden (einschließlich statischer Methoden), Lambdas und Template-Funktionen, die jeweils unterschiedliche Vorteile bieten. Das Implementieren gut organisierter Funktionen umfasst in der Regel die Einhaltung von Prinzipien wie DRY ("Don't Repeat Yourself") und SRP (Single Responsibility Principle), was bedeutet, dass jede Funktion nur eine Sache macht und diese gut macht.

## Siehe auch
Für mehr über Funktionen in C++:
- https://de.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Für Designprinzipien im Zusammenhang mit Funktionen:
- https://de.wikipedia.org/wiki/Single-responsibility_principle
- https://de.wikipedia.org/wiki/Don’t_repeat_yourself

Erfahre mehr über Lambdas und fortgeschrittenen Gebrauch von Funktionen:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
