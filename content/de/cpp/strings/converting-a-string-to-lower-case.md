---
date: 2024-01-20 17:38:02.441241-07:00
description: "Das Umwandeln einer Zeichenfolge in Kleinbuchstaben bedeutet, alle Gro\xDF\
  buchstaben darin in ihre entsprechenden Kleinbuchstaben zu konvertieren.\u2026"
lastmod: '2024-03-13T22:44:54.171932-06:00'
model: gpt-4-1106-preview
summary: "Das Umwandeln einer Zeichenfolge in Kleinbuchstaben bedeutet, alle Gro\xDF\
  buchstaben darin in ihre entsprechenden Kleinbuchstaben zu konvertieren."
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## Was & Warum?
Das Umwandeln einer Zeichenfolge in Kleinbuchstaben bedeutet, alle Großbuchstaben darin in ihre entsprechenden Kleinbuchstaben zu konvertieren. Programmierer verwenden diese Methode oft, um die Benutzereingabe zu normalisieren oder Vergleiche ohne Berücksichtigung von Groß- und Kleinschreibung durchzuführen.

## So geht's:
Mit C++ kannst du die Standardbibliothek `<algorithm>` und die Funktion `std::transform()` nutzen, um eine Zeichenfolge effektiv in Kleinbuchstaben umzuwandeln. Hier ist ein einfaches Beispiel, wie das funktioniert:

```cpp
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string text = "Hallo, Welt!";
    std::transform(text.begin(), text.end(), text.begin(),
        [](unsigned char c) -> unsigned char { return std::tolower(c); });

    std::cout << text << std::endl; // Ausgabe: hallo, welt!
    return 0;
}
```

## Tiefgang:
Früher musste man oft die C-Funktion `tolower` aus der Bibliothek `<ctype.h>` verwenden und jeden Buchstaben der Zeichenfolge einzeln umwandeln. Mit der C++-Standardbibliothek ist die Umwandlung viel einfacher und kann mit der Funktion `std::transform()` zusammen mit `std::tolower()` durchgeführt werden. Es gibt Alternativen wie das Schreiben einer eigenen Schleife oder die Verwendung von Dritt-Bibliotheken, aber `std::transform()` ist eine elegante und idiomatiche C++-Lösung. Dabei ist zu beachten, dass `std::tolower()` aus `<cctype>` Lokalisierungsabhängig sein kann, für rein ASCII-basierte Transformationen ist das jedoch meist unproblematisch.

## Siehe auch:
- C++ Referenz für `std::transform()`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ Referenz für `std::tolower()`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Informationen zur Zeichenlokalisierung in C++: https://en.cppreference.com/w/cpp/locale/locale
