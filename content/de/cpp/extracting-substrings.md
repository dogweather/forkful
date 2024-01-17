---
title:                "Aus dem Extrahieren von Teilzeichenfolgen"
html_title:           "C++: Aus dem Extrahieren von Teilzeichenfolgen"
simple_title:         "Aus dem Extrahieren von Teilzeichenfolgen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Was & Warum?

Es gibt viele Situationen, in denen Programmierer einen Teilstring aus einem vorhandenen String extrahieren müssen. Dies kann nützlich sein, um beispielsweise nur einen Teil einer längeren Zeichenkette zu verwenden oder bestimmte Informationen aus einem Text zu erfassen. Das Extrahieren von Teilstrings ist eine häufige Aufgabe in der Programmierung, die es ermöglicht, effizienter und präziser zu arbeiten.

# Wie?

Die C++-Standardbibliothek bietet eine Reihe von Funktionen, mit denen Teilstrings extrahiert werden können. Eine der einfachsten Möglichkeiten ist die `substr()`-Funktion, die in der `<string>`-Bibliothek enthalten ist. Hier ist ein Beispielcode, der die `substr()`-Funktion verwendet:

```
#include <iostream>
#include <string>
using namespace std;

int main() {
  string text = "Hallo Welt!";
  string teilstring = text.substr(0, 5);
  cout << teilstring << endl;
  // Output: Hallo
  return 0;
}
```

In diesem Beispiel extrahieren wir einen Teilstring aus dem String "Hallo Welt!", indem wir `substr()` mit den Parametern `0` und `5` aufrufen. Der erste Parameter gibt den Startindex an, von dem der Teilstring extrahiert werden soll, und der zweite Parameter gibt die Anzahl der Zeichen an, die extrahiert werden sollen.

# Tiefer tauchen

Das Extrahieren von Teilstrings ist eine gängige Aufgabe in der Programmierung und kann auf verschiedene Weise erreicht werden. Neben der `substr()`-Funktion gibt es auch andere Methoden, die verwendet werden können, wie zum Beispiel die `substr()`-Funktion mit anderen Parametern oder das Verwenden von Iteratoren.

Früher in der C-Programmierung musste man Teilstrings manuell extrahieren, indem man eine for-Schleife benutzte, um jeden Buchstaben einzeln zu überprüfen. Glücklicherweise sind wir heute mit modernen Programmiersprachen und ihren integrierten Funktionen gesegnet, die uns das Leben viel einfacher machen.

Es ist wichtig zu beachten, dass bei der Verwendung von `substr()` der Startindex immer bei `0` beginnt und die Anzahl der Zeichen die Länge des extrahierten Teilstrings widerspiegelt. Wenn das Ende des Teilstrings nicht spezifiziert ist, wird der Rest der Zeichenkette extrahiert.

# Siehe auch

- [C++ Strings: How to extract numbers from a string](https://www.programiz.com/cpp-programming/library-function/cctype/isdigit)
- [C++ Strings: Making substring extraction backwards ](https://en.cppreference.com/w/cpp/string/basic_string/substr)