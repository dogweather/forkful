---
title:    "C++: Erzeugung von Zufallszahlen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Erzeugen von Zufallszahlen ist eine wesentliche Fähigkeit in der Programmierung. Mit Zufallszahlen können wir realistischere Simulationen erstellen, komplexere Probleme lösen und sicherstellen, dass unser Code nicht deterministisch ist. In diesem Blogbeitrag werden wir uns mit der Erzeugung von Zufallszahlen in C++ beschäftigen und zeigen, warum es eine nützliche Fähigkeit ist.

## So geht's

Das Erzeugen von Zufallszahlen in C++ erfordert die Verwendung der <random> Bibliothek. Zunächst müssen wir ein Zufallszahlengenerator-Objekt erstellen, das wir dann verwenden können, um verschiedene Verteilungen von Zufallszahlen zu erstellen.

```C++
#include <iostream>
#include <random>

int main() {

// Erstellen des Zufallszahlengenerator-Objekts
std::default_random_engine generator;

// Erstellen einer Zufallszahl zwischen 1 und 10
std::uniform_int_distribution<int> distribution(1, 10);
int random_number = distribution(generator);

std::cout << "Eine Zufallszahl zwischen 1 und 10: " << random_number << std::endl;
return 0;
}
```

Dieses Beispiel wird immer eine zufällige Ganzzahl zwischen 1 und 10 erzeugen. Durch die Verwendung verschiedener Verteilungen wie uniform_int_distribution, normal_distribution oder exponential_distribution können wir verschiedene Arten von Zufallszahlen erzeugen.

## Tief tauchen

Das Erzeugen von Zufallszahlen ist kein vollständig zufälliger Prozess, sondern basiert auf mathematischen Algorithmen. Diese Algorithmen beginnen mit einem sogenannten "Seed" und verwenden dann verschiedene mathematische Operationen, um eine Folge von Zufallszahlen zu erzeugen.

Es ist wichtig zu beachten, dass bei der Verwendung von Zufallszahlengeneratoren in C++ immer der gleiche Seed verwendet wird, wenn das Programm erneut ausgeführt wird. Wenn wir also eine bestimmte Zufallszahl voraussagen wollen, müssen wir den Seed wissen und ihn beim Erstellen des Zufallszahlengenerators angeben.

Es gibt auch Möglichkeiten, die gleichmäßige Verteilung von Zufallszahlen zu beeinflussen, um bessere Ergebnisse zu erzielen. Dafür gibt es fortgeschrittenere Techniken, aber ein grundlegendes Verständnis dieser Konzepte kann dazu beitragen, die Erzeugung von Zufallszahlen in C++ besser zu verstehen.

## Siehe auch

- [C++ Referenz: Zufallszahlengeneratoren](http://www.cplusplus.com/reference/random/)
- [GeeksForGeeks: Random number generation in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Wikipedia: Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)