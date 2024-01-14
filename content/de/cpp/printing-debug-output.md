---
title:    "C++: Fehlerausgabe ausdrucken"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Ausgeben von Debug-Ausgaben ist ein unverzichtbarer Teil des Programmierprozesses. Es hilft uns dabei, Fehler und Probleme in unserem Code zu erkennen und zu beheben. Ohne Debug-Ausgaben wäre es sehr schwierig, den genauen Verlauf unseres Programms zu verstehen und zu debuggen.

## Wie man Debug-Ausgaben erstellt

Um Debug-Ausgaben in unserem C++ Code zu erstellen, verwenden wir die Standardbibliotheksfunktion `cout`. Wir können wie folgt vorgehen:

```C++
#include <iostream>
using namespace std;

int main(){
    int num = 5;
    cout << "Der Wert von num ist: " << num << endl; // Debug-Ausgabe
    return 0;
}
```

Dieses Beispiel zeigt, wie wir `cout` verwenden, um den Wert einer Variablen auszugeben. Wir können auch Text zusammen mit Variablen ausgeben, um unseren Debugging-Prozess noch weiter zu verbessern.

## Tiefere Informationen über Debug-Ausgaben

Neben der Verwendung von `cout` können wir auch die `assert`-Funktion nutzen, um direkt in unserem Code Ausgaben zu erstellen. Wenn wir `assert` verwenden, geben wir eine Bedingung an, die erfüllt sein muss, damit unser Code weiter ausgeführt wird. Wenn die Bedingung nicht erfüllt ist, gibt `assert` einen Fehler aus und zeigt uns so, an welcher Stelle im Code ein Problem aufgetreten ist.

Um `assert` zu verwenden, müssen wir die Standardbibliotheksfunktion `cassert` einbinden:

```C++
#include <iostream>
#include <cassert>
using namespace std;

int main(){
    int num = 5;
    assert(num == 10); // Debug-Ausgabe mit assert
    return 0;
}
```

Diese Funktion eignet sich besonders gut, um sicherzustellen, dass unsere Variablen die erwarteten Werte haben und somit mögliche Fehler zu vermeiden.

## Siehe auch

- [Die Bedeutung von Debug-Ausgaben für Programmierer](https://blog.teamtreehouse.com/why-debugging-important)
- [Debugging mit Visual Studio](https://docs.microsoft.com/de-de/visualstudio/debugger/get-started-debugging-cpp?view=vs-2019)
- [Tipps und Tricks zum Debugging in C++](https://medium.com/free-code-camp/5-tips-and-tricks-for-debugging-in-c-7723fb0eb3e1)