---
title:    "C++: Erzeugen von Zufallszahlen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Sie als Entwickler zufällige Zahlen in Ihrem Code generieren möchten. Vielleicht erstellen Sie ein Spiel, bei dem der Spieler jedes Mal eine unterschiedliche Erfahrung haben soll. Oder Sie möchten eine Funktion testen, die auf verschiedene Eingaben reagieren muss. In jedem Fall ist die Fähigkeit, zufällige Zahlen zu generieren, ein wichtiger Aspekt der Programmierung.

## Wie geht man vor

Die Erstellung von zufälligen Zahlen in C++ ist relativ einfach. Zunächst müssen Sie die Bibliothek "cstdlib" inkludieren, um die Funktion `rand()` zu verwenden. Dann können Sie die Funktion `srand()` verwenden, um den Startpunkt für die Generierung zufälliger Zahlen festzulegen. Dieser Startpunkt wird auch als "Seed" bezeichnet. Ein üblicher Ansatz ist die Verwendung der aktuellen Systemzeit als Seed. Hier ist ein Beispielcode, der dies demonstriert:

```C++
#include <cstdlib>
#include <iostream>
#include <ctime>

int main() {
    // Seed setzen
    srand(time(0));

    // Zufällige Zahl zwischen 1 und 10 generieren
    int randomNum = rand() % 10 + 1;

    // Ausgabe der generierten Zahl
    std::cout << "Die generierte Zahl ist: " << randomNum << std::endl;

    return 0;
}
```

Dieser Code verwendet `srand()` zusammen mit der Funktion `time()` aus der Bibliothek "ctime", um eine zufällige Zahl zwischen 1 und 10 zu generieren. Sie können jedoch auch den Seed manuell festlegen, indem Sie eine Zahl anstatt von `time(0)` in `srand()` übergeben.

## Tiefergehender Einblick

Zu verstehen, wie das Generieren von zufälligen Zahlen funktioniert, kann helfen, die besten Ansätze für Ihre spezifische Anwendung zu finden. In C++ basiert die Generierung von zufälligen Zahlen auf dem "Pseudozufall" -Algorithmus. Dies bedeutet, dass die Zahlen nicht wirklich zufällig sind, sondern auf einer bestimmten Reihenfolge basieren, die durch den Seed festgelegt wird. Daher ist es wichtig, den Seed regelmäßig zu ändern oder zu diversifizieren, um eine gleichmäßige Verteilung der zufälligen Zahlen zu erreichen. Eine weitere interessante Tatsache ist, dass der Algorithmus normalerweise eine LCG (Linear Congruential Generator) genannt wird und in C++ als "random number engine" implementiert ist.

## Siehe auch

- [C++ Referenz zu `rand()` und `srand()`](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Erklärung des LCG-Algorithmus für die Generierung zufälliger Zahlen](https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap09/random.html)
- [Die Bedeutung von Seeds in der zufälligen Generierung](https://medium.com/@vaibhavagarwal_81727/the-importance-of-seeds-in-random-number-generation-a356a2f9dcda)