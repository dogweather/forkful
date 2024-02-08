---
title:                "Teilstrings extrahieren"
aliases:
- de/cpp/extracting-substrings.md
date:                  2024-01-20T17:45:04.577701-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
### Was & Warum?
Substring-Extraktion ist das Herausgraben von Teilsequenzen aus einem String. Programmierer nutzen sie, um bestimmte Daten zu isolieren oder Formatierungsprobleme zu lösen.

## How to:
### Wie geht das:
```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Grüße aus Berlin";
    
    // Erster Substring: Gibt "Grüße" aus
    std::string teil1 = text.substr(0, 5);
    std::cout << teil1 << std::endl; // "Grüße"
    
    // Zweiter Substring: Gibt "Berlin" aus
    std::string teil2 = text.substr(10);
    std::cout << teil2 << std::endl; // "Berlin"
    
    return 0;
}
```
**Ausgabe:**
```
Grüße
Berlin
```

## Deep Dive:
### Tiefergehend:
Der `.substr()` Befehl ist seit C++98 Standard. Alternativ kann man `std::string::find` mit `std::string::substr` kombinieren, um dynamische Positionen zu nutzen. Beispielsweise, um genau ab dem ersten Leerzeichen zu teilen. Hinsichtlich Performance? `std::string_view` (seit C++17 verfügbar) ist für Leseoperationen effizienter, da es den Originalstring nicht kopiert.

## See Also:
### Siehe auch:
- C++ Standardbibliotheksdokumentation zu `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Stack Overflow Diskussionen zu Substrings in C++: https://stackoverflow.com/questions/tagged/c%2b%2b+substring
- C++17 `std::string_view` Tutorial: https://www.cppstories.com/2017/07/string-view-perf-followup/
