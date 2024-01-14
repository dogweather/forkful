---
title:    "C++: Die Länge eines Strings finden"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Viele Programmierer müssen oft die Länge von Zeichenketten in ihren Programmen ermitteln. Dies kann aus verschiedenen Gründen erforderlich sein, zum Beispiel um die Eingaben der Benutzer zu überprüfen oder um bestimmte Funktionen zu implementieren, die auf der Länge einer Zeichenkette basieren. Egal aus welchem ​​Grund, das Finden der Länge einer Zeichenkette ist eine grundlegende Aufgabe in vielen Programmiersprachen, einschließlich C++.

## How To

Um die Länge einer Zeichenkette in C++ zu finden, verwenden wir die eingebaute Funktion `strlen()`. Diese Funktion erwartet eine Zeichenkette als Eingabe und gibt die Anzahl der Zeichen in der Zeichenkette zurück. Hier ist ein Beispielcode, der die Verwendung von `strlen()` zeigt:

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char name[] = "Max Mustermann";
    int length = strlen(name);
    
    cout << "Die Länge des Namens beträgt: " << length << endl;
    
    return 0;
}
```

Die Ausgabe für dieses Beispiel lautet: `Die Länge des Namens beträgt: 14`. Wie Sie sehen können, ist die Länge der Zeichenkette 14, einschließlich der Leerzeichen und des Abschlussterminals `'\0'`.

## Deep Dive

Die Funktion `strlen()` durchläuft die Zeichenkette, bis sie auf das Abschlussterminal `'\0'` stößt, das am Ende jeder Zeichenkette platziert ist. Sie zählt alle Zeichen, die sie auf dem Weg trifft, und gibt die Gesamtzahl zurück. Beachten Sie, dass `strlen()` nur mit Zeichenketten funktioniert, die mit einem Abschlussterminal enden. Wenn die Zeichenkette kein Abschlussterminal hat, kann die Funktion nicht richtig arbeiten und möglicherweise falsche Ergebnisse liefern. Es ist daher wichtig, sicherzustellen, dass alle verwendeten Zeichenketten korrekt formatiert sind, um unerwartete Fehler zu vermeiden.

## Siehe auch

- [C++ String Tutorial in Deutsch](https://www.programmierenlernenhq.de/c-plus-plus-zeichenketten-string/) 
- [C++ Dokumentation zu `strlen()`](https://www.cplusplus.com/reference/cstring/strlen/)
- [C++ Zeichenketten und ihre Eigenschaften](https://www.geeksforgeeks.org/strings-in-c-2/)