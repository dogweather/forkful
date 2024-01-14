---
title:                "C++: Die Länge eines Strings finden"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende, aber wichtige Fähigkeit beim Programmieren von C++. Ohne diese Fähigkeit ist es schwer, mit Strings effektiv zu arbeiten und ihre Inhalte zu manipulieren. Daher ist es wichtig zu wissen, wie man die Länge eines Strings findet.

## Wie geht das

Um die Länge eines Strings in C++ zu finden, verwenden wir die Funktion `length()` oder `size()`. Diese Funktionen geben die Anzahl der Zeichen in einem String zurück, einschließlich Leerzeichen und Sonderzeichen. Hier ist ein Beispielcode, der das illustriert:

```C++
#include <iostream>
using namespace std;

int main() {
    string name = "Max Mustermann";
    cout << "Der Name hat die Länge " << name.length() << " Buchstaben." << endl;
    return 0;
}

// Output: Der Name hat die Länge 14 Buchstaben.
```

In diesem Beispiel haben wir die Funktion `length()` verwendet, um die Länge des Strings `name` zu finden und sie dann mit einem kurzen Textausdruck ausgegeben. Wir können auch die Funktion `size()` statt `length()` verwenden und das Ergebnis wäre dasselbe.

## Tiefere Einblicke

Wenn wir uns die C++-Dokumentation ansehen, werden wir feststellen, dass `length()` und `size()` keine tatsächlichen Funktionen sind, sondern Methoden, die zur `std::string`-Klasse gehören. Das bedeutet, dass sie nur auf `string`-Objekten funktionieren. Außerdem können wir die Länge eines Strings auch mit der Funktion `strlen()` aus der Header-Datei `cstring` finden. Diese Funktion erwartet jedoch ein C-Zeichenarray als Parameter.

Es ist auch wichtig zu wissen, dass die Länge eines Strings in C++ abhängig von der Verwendung der Zeichenkodierung ist. Wenn wir beispielsweise einen String mit deutschen Umlauten haben, kann die Länge je nach verwendeter Kodierung variieren.

## Siehe auch

- C++ Dokumentation für `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Dokumentation für `strlen()`: https://en.cppreference.com/w/c/string/byte/strlen