---
title:                "C++: Zufallszahlen generieren"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein häufig verwendetes Konzept in der Programmierung, das in verschiedenen Anwendungen nützlich sein kann. Es kann zum Beispiel in Simulationen, Spielen oder in der Kryptographie eingesetzt werden, um eine zufällige und nicht voraussagbare Komponente zu integrieren.

## Wie man Zufallszahlen in C++ generiert

Es gibt verschiedene Ansätze, um Zufallszahlen in C++ zu generieren. Der einfachste davon ist die Verwendung der Standardbibliotheksfunktion `rand()`. Diese Funktion gibt eine Zufallszahl zwischen 0 und einer vom Benutzer angegebenen Obergrenze zurück.

```C++
#include <cstdlib>
#include <iostream>

int main()
{
    // Generiere eine Zufallszahl zwischen 0 und 10
    int random_num = rand() % 11;
    std::cout << "Die Zufallszahl ist: " << random_num << std::endl;
    return 0;
}

// Output: Die Zufallszahl ist: 8
```

Eine bessere Methode ist die Verwendung der C++ Standardbibliothek `random`. Diese bietet verschiedene Funktionen und Distributionen, um Zufallszahlen in einem bestimmten Bereich zu generieren.

```C++
#include <random>
#include <iostream>

int main()
{
    // Erstelle einen Generator und eine gleichmäßige Verteilung zwischen 1 und 10
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 10);
    
    // Generiere eine Zufallszahl zwischen 1 und 10
    int random_num = distr(gen);
    std::cout << "Die Zufallszahl ist: " << random_num << std::endl;
    return 0;
}

// Output: Die Zufallszahl ist: 6
```

Es sollte beachtet werden, dass die Verwendung von `rand()` nicht für kryptographische Zwecke geeignet ist, da die erzeugten Zufallszahlen vorhersagbar sind. Für solche Zwecke gibt es spezielle Pseudozufallszahlengeneratoren, die in der C++ Standardbibliothek vorhanden sind.

## Tiefergehende Informationen

Das Generieren von wirklich zufälligen Zahlen in der Computerwelt ist eine komplexe Aufgabe, da Computer deterministische Maschinen sind. Daher wird bei der Generierung von Zufallszahlen im Allgemeinen ein sogenannter Seed verwendet, der als Ausgangspunkt für die Erzeugung von Pseudozufallszahlen dient. Es ist wichtig, einen guten Seed auszuwählen, um eine gleichmäßige Verteilung der Zufallszahlen zu gewährleisten.

Es gibt auch verschiedene Verteilungen, die für die Generierung von Zufallszahlen verwendet werden können, wie zum Beispiel die Normalverteilung, die Binomialverteilung oder die gleichmäßige Verteilung. Je nach Anwendung sollte die passende Verteilung ausgewählt werden, um realistische Ergebnisse zu erzielen.

## Siehe auch

- [C++ Standardbibliotheksfunktion `rand()`](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [C++ Standardbibliothek `random`](https://en.cppreference.com/w/cpp/header/random)
- [Generierung von Zufallszahlen in C++](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Pseudozufallszahlen und Seed in C++](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [Verteilungen in C++](https://www.geeksforgeeks.org/box-muller-transform-method-for-generating-gaussian-distribution/)