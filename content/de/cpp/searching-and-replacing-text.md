---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Suchen und Ersetzen von Text in C++

## Was & Warum?

Das Suchen und Ersetzen von Text ist ein gängige Operation, bei der in einer Zeichenkette nach bestimmten Text(en) gesucht und durch einen anderen ersetzt wird. Programmierer machen das, um Daten zu manipulieren, Textmuster zu ändern oder Fehler zu beheben.

## So funktioniert's:

Das Folgende ist ein einfacher C++ Code, der die Standardbibliothek verwendet, um nach einem bestimmten Text zu suchen und ihn zu ersetzen.

```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Hallo, Welt!";
    std::string suchen = "Welt";
    std::string ersetzen = "C++";

    size_t pos = text.find(suchen);
    if(pos != std::string::npos)
        text.replace(pos, suchen.length(), ersetzen);

    std::cout << text << std::endl;
}
```

Die Ausgabe wird sein:

```C++
Hallo, C++!
```

## Vertiefende Informationen:

Historisch gesehen wurde das Konzept des Suchens und Ersetzens erstmals in den 1970er Jahren von Texteditoren wie vi und Emacs populär gemacht. In C++ können wir alternativ auch `regex_replace` für reguläre Ausdrücke verwenden. Die `find` und `replace` Methoden verwenden intern den KMP (Knuth-Morris-Pratt)-Algorithmus, der linear läuft.

## Siehe auch:

- C++ Dokumentation zur `std::string`: http://www.cplusplus.com/reference/string/string/
- Einführung in reguläre Ausdrücke in C++: https://en.cppreference.com/w/cpp/regex
- Informationen zum KMP-Algorithmus: https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm