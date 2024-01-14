---
title:    "C++: Textsuche und -ersetzung"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine wichtige Fähigkeit für jeden, der in der Programmierung tätig ist. Es ermöglicht uns, schnell und effizient Änderungen in unseren Code vorzunehmen, ohne manuell jeden einzelnen Abschnitt zu bearbeiten.

## Wie man das macht

Die grundlegendste Methode, Text zu suchen und zu ersetzen, ist die Verwendung der `find()` und `replace()` Funktionen in C++. Diese Funktionen erfordern als Parameter den zu suchenden Text, den Text, durch den er ersetzt werden soll, und die Position, an der die Suche beginnen soll.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string text = "Hallo, wie geht es dir?";
    // Sucht nach dem Text "dir" und ersetzt es mit "mir"
    int pos = text.find("dir");
    text.replace(pos, 3, "mir");

    cout << text << endl;
    // Ausgabe: Hallo, wie geht es mir?
    return 0;
}
```

In diesem Beispiel haben wir den Text "dir" in "mir" geändert. Beachten Sie, dass die `find()` Funktion die Position des gesuchten Textes zurückgibt und die `replace()` Funktion diese Position verwendet, um den Text zu ersetzen.

Es gibt auch fortgeschrittenere Methoden zum Suchen und Ersetzen von Text, wie z.B. die Verwendung von regulären Ausdrücken mit der `regex_replace()` Funktion. Diese erfordert jedoch ein Verständnis von regulären Ausdrücken und ist daher für Anfänger möglicherweise etwas komplex.

## Tiefergehende Informationen

Es ist wichtig zu beachten, dass die `replace()` Funktion nur den ersten gefundenen Text ersetzt. Wenn Sie den gesamten Text durchsuchen und ersetzen möchten, müssen Sie eine Schleife verwenden. Hier ist ein Beispiel, das alle Instanzen von "dir" durch "mir" ersetzt:

```C++
// Schleife, um alle Instanzen von "dir" zu ersetzen
while (pos != string::npos) {
    text.replace(pos, 3, "mir");
    pos = text.find("dir", pos + 1);
}
```

Es ist auch möglich, den Suchtext durch eine leere Zeichenkette zu ersetzen, um ihn effektiv zu löschen.

Eine weitere wichtige Sache zu beachten ist, dass die `find()` Funktion standardmäßig nur nach dem ersten Vorkommen des gesuchten Textes sucht. Um alle Vorkommen zu finden, müssen Sie die `find()` Funktion mit einer Schleife verwenden und die Position mit jedem Durchlauf aktualisieren.

## Siehe auch

- [cppreference - String search](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [cppreference - String replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [TutorialsPoint - C++ Regular Expressions](https://www.tutorialspoint.com/cpp_standard_library/cpp_regular_expressions.htm)