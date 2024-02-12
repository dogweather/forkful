---
title:                "Suchen und Ersetzen von Text"
aliases:
- /de/cpp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:13.454196-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen von Text ist ein Basiskonzept, bei dem Zeichenketten innerhalb einer größeren Textmenge gefunden und durch andere ersetzt werden. Programmierer verwenden es zum effizienten Ändern von Code, Datenaktualisierungen oder zum Verarbeiten von Textdaten.

## Wie geht das?
```C++
#include <iostream>
#include <string>

int main() {
    std::string satz = "Hallo Welt! Hallo Programmierer!";
    std::string suchwort = "Hallo";
    std::string ersatzwort = "Tschüss";

    size_t pos = satz.find(suchwort);
    while(pos != std::string::npos) {
        satz.replace(pos, suchwort.length(), ersatzwort);
        pos = satz.find(suchwort, pos + ersatzwort.length());
    }

    std::cout << satz << std::endl; // Tschüss Welt! Tschüss Programmierer!
    return 0;
}
```

## Tiefere Einblicke
Das Konzept des Suchens und Ersetzens gab es schon lange bevor es moderne Computer gab. Ursprünglich manuell in Texten ausgeführt, wurde es mit der Zeit ein fundamentaler Bestandteil von Texteditoren wie `sed` unter Unix. Heute haben praktisch alle Programmiersprachen eingebaute Funktionen dafür.

In Sachen Alternativen ist `regex` (Regular Expressions) zu nennen. Sie ermöglichen komplexeres Suchen und Ersetzen unter Verwendung von Mustern statt fester Zeichenketten.

Die Implementierung kann trivial oder komplex sein, abhängig von den Anforderungen. Für einfache Fälle genügt oft eine Standard-Funktion, wie `std::string::replace` in C++. Bei komplexeren Mustern oder großer Datenmenge könnte man auf spezialisierte Libraries wie `Boost.Regex` oder sogar parallele Verarbeitung zurückgreifen.

## Siehe auch
- [cplusplus.com - std::string::find](http://www.cplusplus.com/reference/string/string/find/)
- [cplusplus.com - std::string::replace](http://www.cplusplus.com/reference/string/string/replace/)
- [cppreference.com - Regular expressions in C++](https://en.cppreference.com/w/cpp/regex)
- [Boost.Regex](https://www.boost.org/doc/libs/1_76_0/libs/regex/doc/html/index.html)
