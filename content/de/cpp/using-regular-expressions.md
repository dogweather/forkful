---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "C++: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsfähiges Werkzeug in der Welt der Programmierung. Mit ihnen können Sie Texte auf einfache und effiziente Weise durchsuchen und bearbeiten. Reguläre Ausdrücke sparen Zeit und Aufwand und sind deshalb ein unverzichtbares Instrument für jeden Programmierer.

## So geht's

Um reguläre Ausdrücke in C++ zu verwenden, müssen Sie die Header-Datei "regex" einbinden. Dann können Sie mit den integrierten Funktionen "regex_match" und "regex_search" einen regulären Ausdruck auf einen Text anwenden.

Ein Beispiel:
```C++
#include <iostream>
#include <string>
#include <regex>

int main()
{
    std::string text = "Das ist ein Beispieltext.";
    std::regex pattern("Beispiel");

    if (std::regex_search(text, pattern))
    {
        std::cout << "Der Text enthält das Wort 'Beispiel'." << std::endl;
    }
    else
    {
        std::cout << "Der Text enthält NICHT das Wort 'Beispiel'." << std::endl;
    }

    return 0;
}
```

Der ausführliche Output dieses Codes lautet:

```
Der Text enthält das Wort 'Beispiel'.
```

In diesem Beispiel haben wir einen regulären Ausdruck auf einen Text angewendet und mithilfe von "regex_search" überprüft, ob der Text das angegebene Muster enthält. Wenn Sie das nächste Mal einen Text analysieren oder bearbeiten müssen, denken Sie daran, dass reguläre Ausdrücke eine große Hilfe sein können.

## Tiefer tauchen

Reguläre Ausdrücke können nicht nur zur einfachen Suche nach Wörtern oder Mustern verwendet werden, sondern auch zur komplexen Mustererkennung und -manipulation. Sie können Metazeichen verwenden, um Ausdrücke wie "alle Zahlen" oder "alle Großbuchstaben" zu definieren. Wenn Sie mehr über reguläre Ausdrücke erfahren möchten, gibt es im Internet viele Ressourcen und Tutorials, die Ihnen weiterhelfen können.

## Siehe auch

- [C++ regex - cppreference.com](https://en.cppreference.com/w/cpp/regex)
- [Regular Expressions in C++11 - GeeksforGeeks](https://www.geeksforgeeks.org/regular-expressions-in-c/)
- [Mastering Regular Expressions - O'Reilly](http://regex.info/book.html)