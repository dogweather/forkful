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

## Was & Warum?
"Debug Output" oder "Debug-Ausgabe" ist das, was Programmierer verwenden, um Informationen über den Zustand ihres Codes während der Ausführung zu erhalten. Es hilft dabei, Fehler und Probleme zu identifizieren und zu beheben. Debug-Ausgaben werden normalerweise während der Entwicklung von Anwendungen verwendet und anschließend aus dem Code entfernt, bevor er in der Produktionsumgebung eingesetzt wird.

## Wie geht's?
Um Debug-Ausgaben in C++ zu machen, können Sie die `cout` Funktion aus der Standardbibliothek verwenden. Hier ist ein Beispiel, wie Sie eine Debug-Ausgabe mit einer variablen `x` erstellen können:

```C++
#include <iostream>
using namespace std;

int main() {
  int x = 5;
  cout << "Der Wert von x ist: " << x << endl;
  return 0;
}
```

Dieser Code würde die folgende Ausgabe produzieren:

```
Der Wert von x ist: 5
```

## Tiefer einsteigen
Debug-Ausgaben werden seit Beginn der Programmierung verwendet, um Softwarefehler zu identifizieren und zu beheben. Alternativ können auch Debugging-Tools verwendet werden, die speziell für diesen Zweck entwickelt wurden. Eine andere Möglichkeit ist das Verwenden von "Assertions", um bestimmte Bedingungen während der Ausführung des Codes zu überprüfen.

In C++ können Debug-Ausgaben auch mit dem `cerr` Stream statt `cout` erstellt werden. Dies ist nützlich, wenn Ausgaben direkt auf dem Terminal ausgegeben werden müssen. Zum Beispiel:

```C++
#include <iostream>
using namespace std;

int main() {
  cerr << "Achtung: Es ist ein Fehler aufgetreten!" << endl;
  return 0;
}
```

## Siehe auch
- [Debugging 101: Grundlagen des Debuggens](https://www.codingame.com/playgrounds/273/debugging-101-grundlagen-des-debuggens)
- [Debugging in C++](https://www.cprogramming.com/debugging/debugging_c++.html)
- [C++ Debugging Tutorial mit Visual Studio](https://www.learncpp.com/tutorials/debugging-your-code-with-visual-studio/)