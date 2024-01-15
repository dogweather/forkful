---
title:                "Generierung von zufälligen Zahlen"
html_title:           "C++: Generierung von zufälligen Zahlen"
simple_title:         "Generierung von zufälligen Zahlen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum 
Das Generieren von Zufallszahlen ist ein grundlegender Aspekt in der Programmierung und kann in verschiedenen Anwendungsbereichen nützlich sein. Es ermöglicht zum Beispiel die Erstellung von zufällig generierten Passwörtern, die Simulation von Prozessen oder die Erstellung von Testdaten für statistische Analysen.

## How To
Um in C++ zufällige Zahlen zu generieren, kann die Standardbibliotheksfunktion `rand()` verwendet werden. Zunächst muss jedoch die Bibliothek `<cstdlib>` importiert werden. Dann kann die Funktion aufgerufen werden, um eine zufällige Ganzzahl zurückzugeben. Um den Zufallszahlengenerator zu initialisieren, muss `srand()` mit einem Startwert aufgerufen werden, der sich zum Beispiel aus der aktuellen Systemzeit berechnen lässt.

```C++
#include <cstdlib>
#include <iostream>
using namespace std;

int main() {
    // Initialisierung des Zufallszahlengenerators
    srand(time(0));
    
    // Generierung einer zufälligen Ganzzahl zwischen 0 und 10
    int random_num = rand() % 11;
    
    // Ausgabe der Zufallszahl
    cout << "Zufällige Ganzzahl: " << random_num << endl;
}

```

Der obige Code generiert eine zufällige Ganzzahl zwischen 0 und 10 und gibt diese in der Konsole aus. Um eine zufällige Gleitkommazahl zu generieren, kann `rand()` auf ein Gleitkommaintervall skaliert werden, zum Beispiel mit `double random_float = rand() / (1.0 + RAND_MAX);`.

## Deep Dive
In C++ wird der Zufallszahlengenerator durch eine deterministische Funktion implementiert, die eine Sequenz von Pseudozufallszahlen erzeugt. Das bedeutet, dass bei einem festen Startwert immer die gleiche Folge von Zufallszahlen generiert wird. Um dies zu vermeiden, muss der Zufallszahlengenerator jedes Mal mit einem anderen Startwert initialisiert werden, zum Beispiel mit `srand(time(0))` oder `srand(clock())`.

Es ist auch wichtig zu beachten, dass der Zufallszahlengenerator nicht wirklich zufällig ist. Die erzeugte Sequenz folgt einem deterministischen Algorithmus und kann daher durch Vorhersage oder Analyse beeinflusst werden. Es gibt jedoch komplexe Methoden, um die Zufälligkeit zu verbessern, wie zum Beispiel die Verwendung von externen Quellen wie Hardware-Geräten oder Internetdiensten, um den Startwert zu generieren.

## Siehe auch
- [Random number generation in C++ (Englisch)](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Pseudo-random number generation (Englisch)](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [True Random Number Generator in C++ (Englisch)](https://www.geeksforgeeks.org/implement-random-0-6-using-the-given-random-0-1/)