---
title:    "C++: Die Verwendung von regulären Ausdrücken"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug für das Durchsuchen und Manipulieren von Zeichenketten in Ihrem C++ Code. Mit ihnen können Sie komplizierte Muster erkennen und extrahieren, was das Arbeiten mit Texten deutlich einfacher macht.

## Wie geht das?

Um reguläre Ausdrücke in Ihrem C++ Code zu verwenden, müssen Sie zunächst die Bibliothek "regex" in Ihrem Code einbinden. Dann können Sie die Funktionen "regex_match" und "regex_replace" verwenden, um Ihre regulären Ausdrücke auf eine Zeichenkette anzuwenden.

Hier ist ein Beispiel, wie Sie eine Telefonnummer aus einer Zeichenkette extrahieren können:

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string text = "Meine Telefonnummer ist 555-123-4567";
    regex pattern("\\d{3}-\\d{3}-\\d{4}"); // Definiert das Muster einer Telefonnummer
    smatch match; // Übereinstimmender String wird hier gespeichert

    if(regex_search(text, match, pattern)) {
        cout << "Telefonnummer: " << match.str() << endl;
    }
    else {
        cout << "Keine Telefonnummer gefunden." << endl;
    }
    return 0;
}
```

**Ausgabe:**

```
Telefonnummer: 555-123-4567
```

In diesem Beispiel wird die Funktion "regex_search" verwendet, um das Muster auf die Zeichenkette anzuwenden. Die Ausgabe zeigt, dass die Funktion erfolgreich war, und der übereinstimmende Teil der Zeichenkette wird in der Variablen "match" gespeichert.

## Tiefseetauchen

Reguläre Ausdrücke bieten noch viele weitere Möglichkeiten, wie zum Beispiel die Verwendung von quantifizierenden Ausdrücken wie "+" und "*", um mehrere Vorkommnisse eines Musters zu erkennen, oder die Verwendung von Gruppen, um Teile eines Musters zu erfassen.

Es ist auch wichtig zu beachten, dass reguläre Ausdrücke nicht auf Zeichenketten beschränkt sind. Sie können auch auf Eingabeströmen und Dateien angewendet werden.

Sehen Sie sich diese weiterführenden Ressourcen an, um mehr über reguläre Ausdrücke in C++ zu erfahren:

## Siehe auch

- [C++ Reference Guide: Regular Expressions](https://en.cppreference.com/w/cpp/regex)
- [Reguläre Ausdrücke in C++ für Anfänger erklärt](https://www.learncpp.com/cpp-tutorial/regular-expressions/)
- [Offizielle C++ Dokumentation für die Bibliothek "regex"](https://www.cplusplus.com/reference/regex/)