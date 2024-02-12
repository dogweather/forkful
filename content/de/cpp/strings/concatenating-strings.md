---
title:                "Zeichenketten verknüpfen"
aliases:
- /de/cpp/concatenating-strings.md
date:                  2024-01-20T17:34:24.271687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Konkatenation ist das Verknüpfen zweier oder mehrerer Zeichenketten. Programmierer nutzen diese Technik, um dynamische Texte zu erzeugen, Daten zu formatieren oder Benutzereingaben zu verarbeiten.

## How to:
```C++
#include <iostream>
#include <string>

int main() {
    std::string gruss = "Hallo ";
    std::string welt = "Welt!";
    std::string begruessung = gruss + welt;
    
    std::cout << begruessung << std::endl; // Ausgabe: Hallo Welt!
    
    // Anhängen mit += Operator
    std::string frage = "Wie geht's, ";
    frage += "dir?";
    std::cout << frage << std::endl; // Ausgabe: Wie geht's, dir?
    
    return 0;
}
```

## Deep Dive
String-Konkatenation ist so alt wie das Programmieren mit höheren Sprachen selbst. Frühe Programmiersprachen wie C boten dafür low-level Funktionen wie `strcat()` aus der `<cstring>` Bibliothek. C++ vereinfacht den Prozess mit dem `+` Operator und der `std::string` Klasse. Alternativen in C++ sind:
- `stringstream` aus `<sstream>`: Nützlich für komplexe Formattierungen.
- `string::append()`: Äquivalent zum `+=` Operator, aber flexibler.
- `fmt::format` aus der {fmt} Bibliothek (extern): Eine moderne Alternative.

Implementierungsdetails:
- Überladung des `+` Operators ermöglicht einfachen Code.
- Verkettungsoperationen können ineffizient sein, da sie neue Strings erzeugen. Bei großen oder vielen Operationen sind Alternativen zu bevorzugen.
- `std::string` nutzt dynamischen Speicher, was Overhead zur Laufzeit verursacht.

## See Also
- C++ Standardbibliothek: https://www.cplusplus.com/reference/string/string/
- {fmt} Bibliothek: https://fmt.dev/latest/index.html
- Cppreference zu `std::stringstream`: https://en.cppreference.com/w/cpp/io/basic_stringstream
