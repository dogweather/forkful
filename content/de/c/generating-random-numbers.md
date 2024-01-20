---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Erzeugung zufälliger Zahlen in C bedeutet, eine Reihe, die nicht vorhersagbar ist, zu produzieren. Dies ist nützlich für viele Anwendungen, wie Spiele, Verschlüsselung oder Simulationen.


## So geht's:

Zum Erzeugen von Zufallszahlen in C verwenden wir die Funktion `rand()`. Hier ist ein einfacher Weg, wie das gemacht wird:

```C 
#include <stdlib.h> //Funktionen für Zufallszahlen
#include <time.h>   //um die Zeit für srand() zu nutzen

int main() {
    srand(time(0));  //Initialisierung des Zufallsgenerators
    int zufallszahl = rand();  //Erzeugt eine zufällige Zahl
    
    printf("Zufallszahl: %d\n", zufallszahl);

    return 0;
}
``` 
In diesem Code erzeugt die Funktion `rand()` eine Zufallszahl. Die Funktion `srand(time(0))` wird genutzt, um den Zufallsgenerator anders zu initialisieren bei jedem Start des Programms.


## Tiefgang:

Obwohl der Wunsch nach Zufallszahlen so alt ist wie das Spielen selbst, entstanden Computer-basierte Methoden zur Erzeugung von Zufallszahlen erst im 20. Jahrhundert. Die Funktion `rand()` und `srand()` gehören zur Bibliothek stdlib.h, welche Teil der Standard-C-Bibliothek ist und wurde eingeführt in der Norm ANSI C.

Es gibt viele alternative Methoden zur Zufallsgenerierung, wie die Mersenne Twister oder die Linear kongruenze Methode, die in bestimmten Anwendungsgebieten Vorteile haben können.

Die `rand()` Funktion erzeugt deterministische Zufallszahlen, was bedeutet dass sie eigentlich nicht wirklich "zufällig" sind, sondern dieselbe Sequenz von Zahlen erzeugen, wenn sie mit demselben Saatwert (Seed) initialisiert werden. Daher wird `srand(time(0))` verwendet, um einen dynamischen Saatwert bereitzustellen.


## Siehe auch:

1. [C Standard Library - stdlib.h](http://www.cplusplus.com/reference/cstdlib/)
2. [Dokumentation zu rand() und srand()](https://docs.microsoft.com/de-de/cpp/c-runtime-library/reference/rand?view=msvc-160)
3. [Wikipedia über Zufallszahlen](https://de.wikipedia.org/wiki/Zufallszahl)