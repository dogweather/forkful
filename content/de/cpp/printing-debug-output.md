---
title:                "Debug-Ausgabe drucken"
html_title:           "C++: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum
Debugausgabe ist ein wichtiger Teil des Programmierens, da sie dabei hilft, Fehler im Code zu finden und zu beheben. Durch das Drucken von Debugausgaben können Entwickler*innen ihre Programme effektiver und effizienter debuggen und somit schneller zu einem fehlerfreien Ergebnis gelangen.

# Wie funktioniert es?
Das Drucken von Debugausgaben in C++ ist eine einfache Methode, um den Programmierprozess zu unterstützen. Dafür gibt es die Funktion `cout`, die den Text oder die Werte von Variablen auf der Konsole ausgibt. Hier ist ein Beispiel:

```C++
#include <iostream>

using namespace std;

int main(){
    int a = 10;
    float b = 1.5;

    // Debugausgabe
    cout << "Der Wert von a ist: " << a << endl;
    cout << "Der Wert von b ist: " << b << endl;

    return 0;
}

// Ausgabe:
Der Wert von a ist: 10
Der Wert von b ist: 1.5
```

In diesem Beispiel wird der Wert der Variablen `a` und `b` ausgegeben, was hilfreich sein kann, um sicherzustellen, dass die Variablen die gewünschten Werte haben.

# Tiefergehende Informationen
Es gibt verschiedene Möglichkeiten, Debugausgaben in C++ zu nutzen, je nachdem, welchen Teil des Codes Sie überprüfen möchten. Hier sind einige zusätzliche Funktionen, die Sie beim Debuggen verwenden können:

- `cerr`: Gibt Text auf der Fehlerausgabe aus und kann helfen, schwerwiegende Fehler im Programm zu finden.
- ` #ifdef DEBUG`: Diese Bedingung ermöglicht es, Debugausgaben nur im Entwicklungsmodus zu nutzen und sie beim Kompilieren für die endgültige Version zu entfernen.
- `assert()`: Eine Funktion, die es Ihnen ermöglicht, Bedingungen im Code zu überprüfen und zu prüfen, ob diese Bedingungen wahr sind oder nicht.

Es ist wichtig zu beachten, dass Debugausgaben auch den Code verlangsamen können, daher sollten sie nur verwendet werden, wenn sie wirklich benötigt werden.

## Siehe auch
- [Übersicht der C++ Debugging Tools von Microsoft](https://docs.microsoft.com/en-us/cpp/cpp/debugging-tools-for-windows)
- [Tutorial: Debuggen von C++-Code mit Visual Studio Code](https://code.visualstudio.com/docs/cpp/cpp-debug)
- [Tipps und Tricks für effektives Debuggen in C++](https://www.alessandromacor.com/de/how-to-debug-cpp-with-visual-studio-code/)