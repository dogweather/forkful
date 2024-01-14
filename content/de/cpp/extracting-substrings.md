---
title:                "C++: Extrahieren von Teilzeichenfolgen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist ein sehr nützliches Konzept beim Programmieren in C++. Mit Teilstrings können wir bestimmte Teile von Zeichenketten extrahieren, die wir benötigen, und sie dann für verschiedene Zwecke verwenden. Zum Beispiel können wir Teilstrings verwenden, um eine bestimmte Zeichenkette in einer Eingabe zu überprüfen oder um nur einen Teil einer längeren Zeichenkette für die weitere Verarbeitung zu verwenden. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man Teilstrings in C++ extrahieren kann.

## Wie geht das?

Um Teilstrings in C++ zu extrahieren, müssen wir die C++ Standardbibliotheksfunktion `substr()` verwenden. Diese Funktion ermöglicht es uns, einen Teil einer Zeichenkette zu extrahieren, indem wir den Startindex und die Länge des zu extrahierenden Teils angeben. Ein Beispielcode könnte wie folgt aussehen:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Eingabezeichenkette
    string input = "Hallo Welt";

    // Extrahiere den Teilstring "Welt"
    string extracted = input.substr(6, 4);

    // Gib den extrahierten Teilstring aus
    cout << extracted << endl;

    return 0;
}
```

Die Ausgabe des obigen Codes wäre "Welt", da wir ab dem 6. Zeichen (das erste Zeichen hat den Index 0) 4 Zeichen extrahiert haben.

Man kann auch den Startindex weglassen, in diesem Fall wird der Teilstring ab dem Anfang der Zeichenkette extrahiert. Außerdem kann man auch die Länge weglassen, dann wird der Teilstring bis zum Ende der Zeichenkette extrahiert.

## Tiefer eintauchen

Die `substr()`-Funktion gibt uns einen `std::string` zurück, der den extrahierten Teilstring enthält. Dies bedeutet, dass wir mit diesem Teilstring genauso arbeiten können wie mit anderen Zeichenketten, z.B. ihn ausgeben oder ihn mit anderen Zeichenketten verknüpfen. Wir können auch mehrere Teilstrings extrahieren und diese dann kombinieren, um ein bestimmtes Muster in einer Zeichenkette zu finden.

Es gibt auch weitere nützliche Funktionen für die Arbeit mit Teilstrings, wie zum Beispiel `find()`, `rfind()` oder `compare()`. Diese Funktionen ermöglichen es uns, Teilstrings in einer Zeichenkette zu suchen oder Zeichenketten miteinander zu vergleichen. Es lohnt sich also, sich mit diesen Funktionen auseinanderzusetzen, um noch mehr aus Teilstrings herauszuholen.

## Siehe auch

- [C++ substr() function documentation](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [C++ string manipulation functions](https://www.geeksforgeeks.org/string-manipulation-in-c-using-stl/)
- [C++ string library reference](https://www.cplusplus.com/reference/string/)