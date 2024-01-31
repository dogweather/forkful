---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String-Kapitalisierung wandelt alle Buchstaben eines Strings in Großbuchstaben um. Das dient häufig der Benutzerfreundlichkeit oder uniformen Datenverarbeitung.

## How to:

Die C++-Bibliothek `<algorithm>` enthält `std::transform`, das wir hierfür nutzen:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "Hallo Welt!";
    
    std::transform(text.begin(), text.end(), text.begin(), 
                   [](unsigned char c) { return std::toupper(c); });

    std::cout << text << std::endl;  // Ausgabe: HALLO WELT!
    return 0;
}
```

Auch die C-Bibliothek `<cctype>` ist nützlich für `std::toupper`:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main() {
    std::string text = "Servus!";
    for(char &c : text) {
        c = std::toupper(static_cast<unsigned char>(c));
    }

    std::cout << text << std::endl;  // Ausgabe: SERVUS!
    return 0;
}
```

## Deep Dive:

Früher gab es keine Standardmethoden in C++ zur String-Kapitalisierung, also griffen Entwickler auf C-Bibliotheken zurück. Nun bieten moderne C++-Versionen wie C++17 bequeme Algorithmen für diese Operationen.

Alternativ könnten Entwickler ihre eigene Funktion schreiben, um mehr Kontrolle über das Verhalten zu haben, z.B. Lokalisierung oder Sonderfälle. Bei der Implementierung ist Vorsicht geboten, um nicht in die Falle von Lokalisierungsfehlern oder Unicode-Problemen zu tappen - `std::toupper` funktioniert nicht für alle Sprachen oder spezielle Buchstaben.

Mit der Einführung von Bibliotheken wie Boost oder sogar Features im C++17-Standard, sind viele dieser Herausforderungen leichter zu handhaben. Dennoch ist Verständnis über die zugrundeliegenden Prozesse wichtig.

## See Also:

- [cppreference to std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [cppreference to std::toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Boost String Algorithm Library](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
