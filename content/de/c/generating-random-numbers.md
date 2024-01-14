---
title:                "C: Zufallsgenerierung von Zahlen"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Generieren von Zufallszahlen ist ein grundlegender Bestandteil der Programmierung in C. Zufallszahlen werden oft verwendet, um Spiele zu erstellen, Verschlüsselungsmechanismen zu implementieren und Daten zu simulieren. Sie sind auch nützlich, um Entscheidungen zu treffen, wenn ein Programm mehrere verschiedene Pfade durchlaufen kann.

## Wie geht das

Das Generieren von Zufallszahlen in C ist relativ einfach und erfordert nur eine Handvoll grundlegender Funktionen. Zunächst müssen wir die Funktion `srand()` verwenden, um einen sogenannten "Seed" für unsere Zufallszahlen zu setzen. Dieser Seed kann entweder eine nicht negative Ganzzahl sein oder wir können die aktuelle Systemzeit verwenden, um einen zufälligen Seed zu erhalten.

```C
// Seed mit der aktuellen Systemzeit setzen
srand(time(NULL));

// Eine zufällige Ganzzahl zwischen 0 und 99 generieren
int random_number = rand() % 100; 

// Eine zufällige Gleitkommazahl zwischen 0 und 1 generieren
double random_float = rand() / (double)RAND_MAX;
```

Wie Sie sehen können, verwenden wir die Funktion `rand()` in Kombination mit dem Modulo-Operator, um eine Ganzzahl im gewünschten Bereich zu erhalten. Um eine Gleitkommazahl zu generieren, müssen wir den Ausgang von `rand()` durch den Wert von `RAND_MAX` teilen, der normalerweise eine große Ganzzahl ist. Beachten Sie, dass wir `(double)` verwenden, um den Datentyp in einen Gleitkommawert umzuwandeln.

## Tiefer eintauchen

Das Generieren von Zufallszahlen scheint auf den ersten Blick ziemlich einfach zu sein, aber es gibt einige wichtige Dinge, die man im Hinterkopf behalten sollte, um sicherzustellen, dass die Ergebnisse wirklich zufällig sind. Eines der Probleme ist, dass die Funktion `rand()` oft eine "Pseudo-Zufallszahl" generiert, d.h. die generierten Zahlen folgen einem bestimmten Algorithmus und sind somit vorhersehbar. Um dieses Problem zu umgehen, können wir eine Bibliothek wie `librandom` verwenden, die eine bessere Zufälligkeit gewährleistet.

Eine weitere wichtige Überlegung ist die Wiederverwendbarkeit von Seeds. Wenn wir denselben Seed mehrmals verwenden, werden die generierten Zufallszahlen ebenfalls wiederholt. Um dies zu vermeiden, können wir den Seed nach jeder Verwendung aktualisieren, indem wir beispielsweise die aktuelle Systemzeit oder andere sich ständig ändernde Informationen als Seed verwenden.

## Siehe auch

- [Zufallszahlen in C++ generieren](https://www.educative.io/edpresso/how-to-generate-random-numbers-in-cpp)
- [Verschiedene Methoden zur Generierung von Zufallszahlen in C](https://www.geeksforgeeks.org/generating-random-number-range-c/)