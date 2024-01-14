---
title:    "C++: Eine Zeichenfolge in Kleinbuchstaben umwandeln"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem String in Kleinbuchstaben ist ein wichtiger Schritt in der Programmierung, der oft bei der Verarbeitung von Benutzereingaben oder beim Vergleich von Strings verwendet wird. Es ermöglicht eine einheitliche Darstellung von Texten für einfachere Vergleiche und Analysen.

## Wie funktioniert es?

Bevor wir uns in den Code stürzen, müssen wir zunächst verstehen, wie ein String im C++ gespeichert wird. Ein String ist eine Sequenz von Zeichen, die in einem Array gespeichert werden. Jedes Zeichen hat einen numerischen Wert, der als ASCII-Code bezeichnet wird.

Um einen String in Kleinbuchstaben zu konvertieren, müssen wir jeden Buchstaben einzeln durchgehen und seinen ASCII-Code um 32 verringern. Dies führt dazu, dass der Buchstabe in den entsprechenden Kleinbuchstaben umgewandelt wird. Dieser Prozess kann auf verschiedene Arten durchgeführt werden, hier ist ein Beispiel:

```C++
// Beispielcode zum Konvertieren eines Strings in Kleinbuchstaben

#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char string[] = "HALLO";
    
    for (int i = 0; i < strlen(string); i++) {
        string[i] = tolower(string[i]);
    }
    
    cout << string << endl; // Ausgabe: hallo
    
    return 0;
}
```

In diesem Beispiel verwenden wir die Funktion `tolower()`, die in der Header-Datei `<cstring>` enthalten ist, um jeweils jeden Buchstaben in den Kleinbuchstaben zu konvertieren. Der ursprüngliche String wird dabei direkt überschrieben.

## Tiefer Einblick

Obwohl die obige Methode eine einfache Möglichkeit ist, einen String in Kleinbuchstaben zu konvertieren, gibt es auch andere Herangehensweisen. Eine andere Möglichkeit ist die Verwendung der ASCII-Tabelle und die Verwendung von Bit-Manipulationen, um die Buchstaben in Kleinbuchstaben umzuwandeln.

Ein weiteres wichtiges Detail beim Konvertieren von Strings ist, dass es Unicode- und Mehrbyte-Zeichen gibt, die nicht mit der ASCII-Tabelle übereinstimmen. In diesen Fällen muss ein anderer Ansatz gewählt werden, um die Zeichen korrekt zu konvertieren.

## Siehe auch

- [C++ String Operations](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [ASCII Table](https://www.asciitable.com/)
- [Unicode and UTF-8](https://www.cs.princeton.edu/courses/archive/spr11/cos436/UTF-8.html)