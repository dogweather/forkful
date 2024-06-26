---
date: 2024-01-26 03:37:49.058970-07:00
description: "Wie geht das: Hier ist eine unkomplizierte Methode, um in C++ diese\
  \ Anf\xFChrungszeichen loszuwerden."
lastmod: '2024-03-13T22:44:54.172977-06:00'
model: gpt-4-0125-preview
summary: "Hier ist eine unkomplizierte Methode, um in C++ diese Anf\xFChrungszeichen\
  \ loszuwerden."
title: "Anf\xFChrungszeichen aus einem String entfernen"
weight: 9
---

## Wie geht das:
Hier ist eine unkomplizierte Methode, um in C++ diese Anführungszeichen loszuwerden:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hallo, 'Welt'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Führen Sie dies aus, und Sie erhalten:

```
Hallo, Welt!
```

Voilà! Die Anführungszeichen sind verschwunden.

## Tiefere Einblicke
Anführungszeichen sind seit den Anfängen der Computertechnik ein Textärgernis. Früher sah man Programmierer mühsam durch jeden Buchstaben schleifen, um diese Anführungszeichen zu filtern. Heutzutage haben wir `std::remove` in der Standardvorlagenbibliothek (STL), um die harte Arbeit zu übernehmen.

Alternativen? Sicher! Sie könnten reguläre Ausdrücke mit `std::regex` verwenden, um Anführungszeichen gezielt anzugehen, aber das ist ein bisschen so, als würde man mit einem Vorschlaghammer eine Nuss knacken - kraftvoll, aber für einfache Aufgaben überdimensioniert. Für diejenigen, die die neueren C++-Varianten bevorzugen, könnten Sie mit `std::string_view` für nicht-modifizierende Ansätze experimentieren.

Implementierungstechnisch betrachtet, entfernt `std::remove` tatsächlich keine Elemente aus dem Container; es sortiert nicht entfernte Elemente nach vorne und gibt einen Iterator hinter das neue Ende der Auswahl zurück. Deshalb benötigen wir die `erase` Methode, um den ungewünschten Schwanz abzuschneiden.

## Siehe auch
- C++ `std::remove` Referenz: [cppreference.com](https://de.cppreference.com/w/cpp/algorithm/remove)
- Mehr über `std::string` Manipulation: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
