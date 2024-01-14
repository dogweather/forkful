---
title:                "C++: Fehlersuch-Ausgaben ausgeben"
simple_title:         "Fehlersuch-Ausgaben ausgeben"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
Debug-Ausgaben sind ein wichtiger Bestandteil der C++-Programmierung und können dabei helfen, Fehler zu finden und zu beheben. Es ist eine einfache Methode, um den Code während der Entwicklung zu überprüfen und zu verstehen, wie das Programm innerhalb des Codes arbeitet. Es ermöglicht auch, das Verhalten des Programms zu überwachen und Feedback zu erhalten, um es besser zu optimieren.

## Wie geht es
Um Debug-Ausgaben in C++ zu verwenden, können Sie die Funktion `std::cout` aus der Standardbibliothek verwenden. Diese Funktion ermöglicht es, Daten in die Konsole auszugeben. Sie benötigen auch die Header-Datei `iostream` in Ihrem Code, damit die Funktion ordnungsgemäß funktioniert. Schauen wir uns ein Beispiel an:

```C++
#include <iostream>

int main() {
    int zahl = 10;
    std::cout << "Die Zahl ist: " << zahl << std::endl;
    return 0;
}
```

In diesem Beispiel wird die Variable `zahl` initialisiert und der Wert 10 zugewiesen. Mit `std::cout` können wir dann die Nachricht "Die Zahl ist: " zusammen mit dem Wert von `zahl` in der Konsole ausgeben. Die Verwendung von `std::endl` ermöglicht auch, dass die Ausgabe in einer neuen Zeile erfolgt.

## Tiefer Abstieg
Obwohl der Einsatz von `std::cout` recht einfach ist, gibt es verschiedene Möglichkeiten, sie zu verwenden, um die Ausgabe auf verschiedene Weise zu formatieren und zu gestalten. Zum Beispiel können Sie mehrere Variablen in einer Ausgabezeile verwenden, indem Sie sie mit dem `<<`-Operator trennen. Sie können auch verschiedene Datentypen wie Strings, Integer, Gleitkommazahlen usw. kombinieren.

Eine weitere Möglichkeit, Debug-Ausgaben zu verwenden, ist die Verwendung von Konditionalen, um nur unter bestimmten Bedingungen Ausgaben zu machen. Dies kann hilfreich sein, um die Ausgabe zu vermeiden, wenn sie nicht benötigt wird, und somit den Code effizienter zu gestalten.

## Siehe auch
- [C++ Debugging-Tutorial auf YouTube](https://www.youtube.com/watch?v=nlYs5kBR5Ds)
- [Verwendung von `std::cout` in C++](https://www.geeksforgeeks.org/using-stdcout-separately-in-cpp/)
- [Debugging in C++ mit Code::Blocks](https://www.codingunit.com/cplusplus-tutorial-3-2-the-great-mystery-of-cout)