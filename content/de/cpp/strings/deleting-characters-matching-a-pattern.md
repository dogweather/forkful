---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
aliases: - /de/cpp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:06.431703-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Zeichen aus einem String zu entfernen, die einer spezifischen Regel folgen. Programmierer nutzen diese Technik, um Daten zu bereinigen, Eingaben zu validieren oder Strings vor der Verarbeitung zu formatieren.

## So geht's:
Stellen wir uns vor, wir möchten alle Ziffern aus einem String entfernen. Wir verwenden `<regex>` für Musterabgleich und `<algorithm>` für `std::remove_if`.

```C++
#include <iostream>
#include <string>
#include <regex>
#include <algorithm>

int main() {
    std::string text = "C++11 wurde im Jahr 2011 veröffentlicht!";
    std::regex pattern("\\d");

    text = std::regex_replace(text, pattern, "");
    std::cout << text << std::endl;  // Ausgabe: "C++ wurde im Jahr  veröffentlicht!"

    return 0;
}
```

Oder, ohne Regex, indem man `std::remove_if` und `std::isdigit` nutzt:

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string text = "C++20 wurde im Dezember 2020 eingeführt.";
    text.erase(std::remove_if(text.begin(), text.end(),
                              [](unsigned char c){ return std::isdigit(c); }),
               text.end());
    std::cout << text << std::endl;  // Ausgabe: "C++ wurde im Dezember  eingeführt."

    return 0;
}
```

## Tiefere Einblicke
Das Löschen von Zeichen, die einem Muster entsprechen, hat sich seit den frühen Tagen der Programmierung entwickelt. Früher mussten Programmierer solche Aufgaben manuell mit Schleifen durchführen. Mit C++11 führte die Standardbibliothek `<regex>` ein, was regelbasierte String-Manipulation vereinfacht.

Alternativen zum Pattern-Deletion sind unter anderem Bibliotheken von Drittanbietern oder manchmal sogar benutzerdefinierte Parsing-Funktionen, falls die Standardbibliothek nicht ausreicht.

In der Implementierung ist es wichtig, auf Performance zu achten. `std::regex` kann langsamer sein als ein gut geschriebener, manueller Ansatz. `std::remove_if` in Kombination mit `std::isdigit` ist oft schneller, leidet aber unter mangelnder Flexibilität gegenüber komplexen Mustern. In den meisten Fällen ist Klarheit und Wartbarkeit wichtiger als Perfektion in der Performance.

## Weiterführende Ressourcen
- [cplusplus.com - regex](http://www.cplusplus.com/reference/regex/)
- [cppreference.com - std::remove_if](https://en.cppreference.com/w/cpp/algorithm/remove)
- [cppreference.com - Input/Output Library](https://en.cppreference.com/w/cpp/header/iostream)
