---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# C++ Zufallszahlen Generieren - Eine In-depth Anleitung

## Was & Warum?

Zufallszahlen in der Programmierung sind nützlich um zufällige Ereignisse zu simulieren oder um Varianz in ein sonst vorhersagbares System zu bringen. Mit ihnen kann man Krypto-Systeme sicherer machen oder ein unvorhersehbares Benutzererlebnis schaffen.

## Wie geht das?

Beginnen wir mit dem einfachen Weg. Mit C++ können Sie in nur wenigen Zeilen Code Zufallszahlen generieren.

```C++
#include <cstdlib>
#include <ctime>
#include <iostream>

int main() {
    srand((unsigned) time(0));
    for(int i = 0; i < 10; i++) {
        std::cout << "Random value: " << rand() % 100 << std::endl;
    }

    return 0;
}
```

Wenn Sie dieses Code-Beispiel ausführen, erhalten Sie zehn Zufallszahlen zwischen 0 und 99.

## Deep Dive

Frühe Zufallszahlengeneratoren basierten oft auf Pseudozufallszahlen. Diese Zahlen sind eigentlich nicht wirklich zufällig, da sie von einem Anfangswert oder "Seed" abhängen. Ändern Sie den Seed, und Sie erhalten eine andere Zahlenfolge, aber dieselbe Seed führt immer zur selben Folge.  

C++ bietet auch komplexere Möglichkeiten zur Generierung von Zufallszahlen. Ein Beispiel dafür ist die `<random>` Bibliothek. Hier ist ein Codebeispiel:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(0, 99);

    for (int i = 0; i < 10; i++) {
        std::cout << "Random value: " << distr(gen) << std::endl;
    }

    return 0;
}
```

Diese Version verwendet Mersenne Twister (`std::mt19937`), eine beliebte Methode zum Generieren von pseudozufälligen Zahlen.

## Siehe auch

1. *cppreference.com* zu `<random>`: [http://en.cppreference.com/w/cpp/numeric/random](http://en.cppreference.com/w/cpp/numeric/random)
2. *cplusplus.com* zu `rand()`: [http://www.cplusplus.com/reference/cstdlib/rand/](http://www.cplusplus.com/reference/cstdlib/rand/)
3. *Wikipedia*-Artikel über Zufallszahlen: [https://de.wikipedia.org/wiki/Zufallszahl](https://de.wikipedia.org/wiki/Zufallszahl)