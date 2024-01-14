---
title:    "C: Zufallszahlen generieren"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist in vielen Anwendungen von großer Bedeutung. Sie können zum Beispiel zum Testen von Programmen oder zur Erzeugung von Passwörtern verwendet werden. In dieser Blog-Post werden wir uns ansehen, wie man mithilfe von C Zufallszahlen generieren kann.

## Wie man Zufallszahlen in C generiert

Um Zufallszahlen in C zu generieren, können wir die Funktion `rand()` aus der Standard-Bibliothek `stdlib.h` verwenden. Diese Funktion gibt eine zufällige Ganzzahl zurück. Um den Zufallszahlengenerator zu initialisieren, können wir die Funktion `srand()` verwenden, die als Argument eine Startzahl nimmt.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    int i;

    // Initialisiere den Zufallszahlengenerator
    srand(42);

    // Generiere 10 Zufallszahlen und gebe sie aus
    for(i = 0; i < 10; i++){
        printf("%d\n", rand());
    }

    return 0;
}
```

Die Ausgabe könnte zum Beispiel so aussehen:

```
1608637542
1476086149
1403506687
725210726
2005242322
393473520
261768637
628175011
659139015
979887933
```

## Tiefentauchen

In C wird der Zufallszahlengenerator durch einen linearen Kongruenzgenerator implementiert. Dieser verwendet eine mathematische Formel, um jedes Mal eine neue Zufallszahl zu generieren. Die Zufallszahl wird durch eine Modulorechnung mit einer großen Zahl begrenzt, um eine Ganzzahl zurückzugeben. Dadurch wird sichergestellt, dass die generierten Zufallszahlen gleichmäßig verteilt sind.

Es gibt auch eine Möglichkeit, die Bereiche der Zufallszahlen zu ändern. Zum Beispiel können wir den Bereich von 0 bis 10 ändern, indem wir die generierte Zahl durch 11 teilen und den Rest der Division verwenden. Dies würde uns Zufallszahlen zwischen 0 und 10 geben.

## Siehe auch

- [rand() Funktion in der C-Dokumentation](https://de.cppreference.com/w/c/numeric/random/rand)
- [srand() Funktion in der C-Dokumentation](https://de.cppreference.com/w/c/numeric/random/srand)
- [Lineare Kongruenzgeneratoren in der Wikipedia](https://de.wikipedia.org/wiki/Lineare_Kongruenzmethode)