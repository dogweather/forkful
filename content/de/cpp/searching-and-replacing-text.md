---
title:                "C++: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren geht es oft darum, Text in einer Datei zu bearbeiten. Dies kann auf verschiedene Weise geschehen, aber eine gängige Methode ist die Suche und Ersetzung von Text. In diesem Blogbeitrag werden wir uns genauer anschauen, wie dies in C++ funktioniert und welche Vorteile es bietet.

## Wie man Text durchsucht und ersetzt

Um Text in C++ zu durchsuchen und zu ersetzen, verwenden wir die Standardbibliotheksfunktion `find()` und `replace()`. Diese Funktionen durchsuchen einen Text nach einem bestimmten Muster und ersetzen es durch einen neuen Text.

```C++
#include <iostream>
#include <string>

int main() {
  // Eingabe des Textes
  std::string text = "Dies ist ein Beispieltext.";

  // Durchsuchen und ersetzen des Textes
  size_t pos = text.find("Beispieltext");
  text.replace(pos, 13, "neuer Text");

  // Ausgabe des Ergebnisses
  std::cout << text << std::endl;
}
```

Das obige Beispiel durchsucht den Text nach dem Muster "Beispieltext" und ersetzt es durch "neuer Text". Die `find()`-Funktion gibt die Position des gefundenen Musters zurück, die dann an `replace()` übergeben wird, um den Text zu ersetzen. Die Zahl 13 in `replace()` gibt die Länge des zu ersetzenden Musters an.

Die Ausgabe des obigen Beispiels wäre: "Dies ist ein neuer Text." Dies ist nur ein einfaches Beispiel, aber mit dieser Methode können auch komplexere Muster gesucht und ersetzt werden.

## Tiefergehende Informationen über die Suche und Ersetzung von Text

Es gibt noch weitere Funktionen und Möglichkeiten, um Text in C++ zu durchsuchen und zu ersetzen. Zum Beispiel können mit `find_first_of()` und `find_last_of()` bestimmte Zeichen im Text gesucht werden. Die Funktion `replace()` kann auch verwendet werden, um nicht nur einen Text in einem Satz, sondern auch in mehreren Sätzen zu ersetzen.

Es gibt auch die Möglichkeit, reguläre Ausdrücke zu verwenden, um nach Mustern zu suchen und diese zu ersetzen. Dies ermöglicht noch flexiblere Suchanfragen und ersetzt auch komplexere Muster.

## Siehe auch

- [C++ Standardbibliothek](https://de.cppreference.com/w/cpp/header)
- [Suche und Ersetzung mit String-Funktionen](https://www.geeksforgeeks.org/cpp-string-class-and-its-applications/)