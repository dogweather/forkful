---
aliases:
- /de/cpp/removing-quotes-from-a-string/
date: 2024-01-26 03:37:49.058970-07:00
description: "Anf\xFChrungszeichen aus einem String zu entfernen bedeutet, diese l\xE4\
  stigen doppelten oder einzelnen Zeichen, die unseren Text umgeben (' oder \"),\u2026"
lastmod: 2024-02-18 23:09:05.176541
model: gpt-4-0125-preview
summary: "Anf\xFChrungszeichen aus einem String zu entfernen bedeutet, diese l\xE4\
  stigen doppelten oder einzelnen Zeichen, die unseren Text umgeben (' oder \"),\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
---

{{< edit_this_page >}}

## Was & Warum?
Anführungszeichen aus einem String zu entfernen bedeutet, diese lästigen doppelten oder einzelnen Zeichen, die unseren Text umgeben (' oder "), wegzunehmen. Programmierer tun dies oft, um Eingaben zu bereinigen, Text in einer Datenbank zu speichern oder Strings für die weitere Verarbeitung ohne das Durcheinander von Anführungszeichen vorzubereiten.

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
