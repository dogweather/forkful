---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:30.338336-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen in C++ zu generieren, heißt, nicht vorhersehbare Werte zu erzeugen, die bei jedem Programmstart variieren können. Das ist entscheidend für Funktionen wie Spielelogik, Simulationen und Sicherheitsalgorithmen.

## How to:
Generiere Zufallszahlen in C++ mit `#include <random>`:

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd; 
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<> distr(1, 100); 

    std::cout << "Zufallszahl: " << distr(gen) << std::endl;

    return 0;
}
```
Beispiel Ausgabe:
```
Zufallszahl: 42
```

## Deep Dive:
Früher nutzte man `rand()` und `srand()` aus `<cstdlib>`, aber diese waren oft nicht wirklich zufällig und nicht gut für alle Anwendungsfälle. Die modernen C++-Bibliotheken wie `<random>` bieten bessere Algorithmen, wie den Mersenne Twister (`std::mt19937`). Für nicht-ganzzahlige Werte gibt es `std::uniform_real_distribution<>`. Implementierungs-Details sind komplex, aber wichtig ist, dass du den Zufallszahlengenerator (`std::mt19937` zum Beispiel) einmal mit einer guten "Seed"-Quelle wie `std::random_device` initialisierst, und dann für die Zufallszahlen brauchst.

## See Also:
- C++ Standardbibliotheksdokumentation: https://en.cppreference.com/w/cpp/header/random
- C++11 Zufallszahlentutorial: https://www.learncpp.com/cpp-tutorial/random-number-generation/
- Artikel zum Thema Pseudozufallszahlengeneratoren: https://www.random.org/randomness/
