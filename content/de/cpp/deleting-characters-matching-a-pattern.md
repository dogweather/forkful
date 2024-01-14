---
title:                "C++: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Warum

Beim Programmieren gibt es oft die Notwendigkeit, bestimmte Zeichen in einem Text oder einer Zeichenkette zu entfernen. Sei es, weil sie überflüssig sind oder weil sie einer bestimmten Formatierung entsprechen müssen. Das Löschen von Zeichen anhand eines bestimmten Musters kann sehr nützlich sein, um Texte effizient zu bearbeiten und zu formatieren.

##Anleitung

Um Zeichen anhand eines Musters zu löschen, gibt es mehrere Möglichkeiten in C++. Eine Möglichkeit ist die Verwendung von Schleifen und Bedingungen, um jeden Buchstaben in einem Text zu überprüfen und gegebenenfalls zu löschen. Eine andere Möglichkeit ist die Verwendung von eingebauten Funktionen, wie zum Beispiel der replace() Funktion aus der Standardbibliothek <string>. Hier ist ein Beispiel, wie man alle Leerzeichen in einem Text löschen kann:

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string text = "Das ist ein Beispieltext.";
    for (int i = 0; i < text.length(); i++) {
        if (text[i] == ' ') {
            text.replace(i, 1, "");
        }
    }
    cout << text << endl;
}
```

Dieses Beispiel nutzt eine Schleife, um jeden Buchstaben in der Variable "text" zu überprüfen. Wenn ein Leerzeichen gefunden wird, wird es mit der replace() Funktion gelöscht. Das Ergebnis wäre:

```
DasisteinBeispieltext.
```

Natürlich können auch andere Muster anstelle von Leerzeichen verwendet werden, je nachdem welche Zeichen gelöscht werden sollen. Es ist auch möglich, reguläre Ausdrücke mit der <regex> Bibliothek zu verwenden.

##Tiefer Einblick

Beim Löschen von Zeichen anhand eines Musters gibt es einige Dinge zu beachten. Zum einen sollte man immer darauf achten, dass nur die gewünschten Zeichen gelöscht werden und keine anderen unerwarteten Zeichen im Text verändert werden. Außerdem ist es wichtig zu bedenken, dass das Löschen von Zeichen auch die Länge des Textes beeinflusst und somit eventuell weitere Anpassungen im Code erforderlich sind.

Es ist auch wichtig zu beachten, dass das Löschen von Zeichen in einer Schleife sehr zeitaufwändig sein kann, insbesondere bei längeren Texten. Daher ist es immer ratsam, nach effizienteren Methoden zu suchen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder eingebauten Funktionen.

##Siehe auch

- [std::string::replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [C++ Regular Expressions](https://en.cppreference.com/w/cpp/regex)
- [C++ string mit Schleifen bearbeiten (German)](https://www.programmierenlernen24.de/cpp-string-mit-schleifen-bearbeiten/)