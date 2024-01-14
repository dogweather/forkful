---
title:                "C++: Zufallszahlen erzeugen"
simple_title:         "Zufallszahlen erzeugen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist in der Programmierung oft sehr nützlich. Sie können verwendet werden, um zufällige Entscheidungen zu treffen, Simulationen zu erstellen oder Daten zu verschlüsseln. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in C++ zufällige Zahlen erzeugen kann.

## Wie geht man vor?

Um zufällige Zahlen in C++ zu generieren, können wir die Standardbibliotheksfunktion `rand()` verwenden. Diese Funktion liefert eine zufällige Ganzzahl zwischen 0 und der größten darstellbaren Ganzzahl. Um die Zahlen in einem bestimmten Bereich zu begrenzen, können wir den Modulo-Operator `%` verwenden.

```C++
#include <iostream>
#include <cstdlib> // Für die Verwendung von rand()

int main() {
    int zufall = rand() % 100; // Zufallszahl zwischen 0 und 99
    std::cout << "Die zufällige Zahl ist: " << zufall << std::endl;
    return 0;
}
```

Die Ausgabe könnte zum Beispiel "Die zufällige Zahl ist: 47" sein.

## Tiefergehende Erklärung

Die Funktion `rand()` generiert Zufallszahlen basierend auf einem sogenannten "Zufalls-Seed". Standardmäßig wird für den Seed die aktuelle Uhrzeit verwendet, was bedeutet, dass bei jedem Durchlauf des Programms eine andere Zufallszahl erzeugt wird. Wir können jedoch auch einen bestimmten Seed mit der Funktion `srand()` festlegen. Dadurch können wir beispielsweise bei jedem Durchlauf des Programms dieselbe Zufallszahl erhalten.

Falls wir jedoch echte Zufälligkeit benötigen, sollten wir einen speziellen Zufallszahlengenerator verwenden, der in der C++11-Standardbibliothek enthalten ist. Ein Beispiel hierfür ist `mt19937`, der auf dem Mersenne-Twister-Algorithmus basiert und als deutlich zufälliger gilt als `rand()`.

## Siehe auch

- [cppreference - rand()](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [cppreference - mt19937](https://en.cppreference.com/w/cpp/numeric/random/mersenne_twister_engine)