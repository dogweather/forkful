---
title:                "Zufallszahlen generieren"
html_title:           "C: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Erzeugung von Zufallszahlen beschäftigen? Nun, Zufallszahlen sind in der Programmierung äußerst nützlich, insbesondere für Spiele, Kryptografie und Simulationen. Mit zufälligen Werten können verschiedene Ergebnisse erzielt werden und somit die Vielfalt und Komplexität von Programmen erhöht werden.

## Wie geht das?

Zufallszahlen in C zu erzeugen ist relativ einfach. Zuerst müssen wir die ```stdlib.h```-Bibliothek einbinden, die einige Funktionen zur Zufallserzeugung enthält. Dann können wir die ```srand()```-Funktion verwenden, um einen sogenannten "Seed" zu setzen. Der Seed bestimmt den Startwert für den Zufallsgenerator und sollte bei jedem Programmstart unterschiedlich sein. Hier ist ein Beispielcode:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Wir setzen den Seed auf die aktuelle Uhrzeit
    srand(time(0));

    // Jetzt können wir mit der Funktion rand() Zufallszahlen erhalten
    // Zum Beispiel geben wir 10 zufällige Zahlen zwischen 1 und 100 aus
    for(int i = 0; i < 10; i++) {
        // Mit dem Modulo-Operator (%) können wir den Wertebereich begrenzen
        // In diesem Fall rechnen wir die Zufallszahl modulo 100, um sie zwischen 0 und 99 zu erhalten
        int randomNumber = rand() % 100;
        // Um die Zahl zwischen 1 und 100 zu erhalten, addieren wir 1
        randomNumber += 1;

        // Ausgabe der Zufallszahl
        printf("%d\n", randomNumber);
    }

    return 0;
}
```

Die Ausgabe könnte zum Beispiel so aussehen:

```
34
5
91
78
11
42
83
99
68
24
```

## Tiefer geht's

Die ```rand()```-Funktion in C erzeugt Pseudozufallszahlen, die auf dem Seed basieren. Das heißt, die generierten Zahlenfolgen sind bei jedem Programmstart gleich. Um wirklich zufällige Zahlen zu erhalten, können wir die Funktion ```random()``` aus der ```unistd.h```-Bibliothek verwenden. Diese Funktion basiert auf einem sogenannten "Random Seed Generator" und erzeugt jede Sekunde einen neuen Seed. Dadurch sind die erzeugten Zahlenfolgen bei jedem Programmstart unterschiedlich.

Außerdem ist es möglich, die Zufallsgeneratorfunktion mit bestimmten Algorithmen zu verbessern, um eine noch bessere Verteilung der Zufallszahlen zu erreichen. Eine bekannte Methode ist die "Mersenne Twister"-Methode, die in der ```<math.h>```-Bibliothek enthalten ist.

## Siehe auch

- [Offizielle C-Dokumentation zur Zufallserzeugung](https://en.cppreference.com/w/c/numeric/random)
- [Tutorial für die Verwendung von Zufallszahlen in C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Echte Zufallszahlen in C erzeugen](https://www.educative.io/edpresso/how-to-generate-random-numbers-in-c)