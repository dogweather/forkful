---
title:    "C++: Umwandeln einer Zeichenfolge in Kleinbuchstaben"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum jemand sich dafür entscheiden könnte, einen String in Kleinbuchstaben umzuwandeln. Möglicherweise müssen Benutzereingaben validiert werden, um sicherzustellen, dass sie nicht case sensitive sind. Oder es kann auch verwendet werden, um Strings für den Vergleich zu normalisieren. Egal aus welchem Grund, die Konvertierung eines Strings in Kleinbuchstaben ist eine grundlegende Fähigkeit, die jeder Programmierer beherrschen sollte.

## Wie man es macht

Um einen String in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze. Eine Möglichkeit ist die Verwendung der `transform()` Funktion aus der Standard Template Library (STL). Schauen wir uns ein Beispiel an:

```C++
#include <iostream>
#include <algorithm> // Für std::transform
#include <string> // Für std::string

using namespace std;

int main() {
    // String definieren
    string name = "GUSTAV";

    // String in Kleinbuchstaben umwandeln
    transform(name.begin(), name.end(), name.begin(), ::tolower); // ::tolower ist ein Funktionszeiger

    // Ausgabe
    cout << name << endl;
    
    return 0;
}

// Output: gustav
```

In diesem Beispiel verwenden wir `transform()` zusammen mit dem `tolower` Funktionszeiger, um jede einzelne Buchstabe des Strings in Kleinbuchstaben umzuwandeln. Beachten Sie, dass wir `name.begin()` und `name.end()` als Bereich für die `transform()` Funktion angeben. Das Ergebnis wird direkt im `name` String gespeichert.

## Tiefergehende Erklärung

Die `transform()` Funktion gehört zur Algorithmusbibliothek der STL und akzeptiert drei Argumente: einen Startbereich, einen Endbereich und eine Zielzeichenkette, in die das Ergebnis gespeichert wird. Der optionale vierte Argument ist eine Funktionszeiger, die angibt, welche Operation auf jedes Element im Bereich angewendet wird. In unserem Beispiel verwenden wir `::tolower`, ein vordefinierter Funktionszeiger aus der `<string.h>` Header-Datei, um jeden Buchstaben in Kleinbuchstaben umzuwandeln.

Es ist auch möglich, eine benutzerdefinierte Funktion zu verwenden, anstatt den `tolower` Funktionszeiger. Hierbei macht es Sinn, eine Funktion mit einer char übergabe zu erstellen, anstatt eine Referenz auf einen char. Dies liegt daran, dass der `transform()` Algorithmus ein Element des Bereichs als Kopie übergeben wird. Aus diesem Grund sollte die Funktion eine Kopie akzeptieren, auch wenn Sie eine Referenz auf char als Argument verwenden.

## Siehe auch

- [C++ Standard Template Library (STL)](https://de.wikibooks.org/wiki/C%2B%2B-Programmierung:_STL)
- [std::transform Dokumentation](https://www.cplusplus.com/reference/algorithm/transform/)