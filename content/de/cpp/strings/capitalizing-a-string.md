---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:03.516256-07:00
description: "Wie geht das: In C++ k\xF6nnen Sie einen String unter Verwendung der\
  \ Standardbibliothek kapitalisieren, ohne dass Bibliotheken von Drittanbietern ben\xF6\
  tigt\u2026"
lastmod: '2024-03-13T22:44:54.167891-06:00'
model: gpt-4-0125-preview
summary: "In C++ k\xF6nnen Sie einen String unter Verwendung der Standardbibliothek\
  \ kapitalisieren, ohne dass Bibliotheken von Drittanbietern ben\xF6tigt werden."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie geht das:
In C++ können Sie einen String unter Verwendung der Standardbibliothek kapitalisieren, ohne dass Bibliotheken von Drittanbietern benötigt werden. Für komplexere oder spezifische Kapitalisierungsverhaltensweisen können jedoch Bibliotheken wie Boost recht hilfreich sein. Unten finden Sie Beispiele, die beide Ansätze veranschaulichen.

### Verwendung der Standard-C++-Bibliothek:
```cpp
#include <iostream>
#include <cctype> // für std::tolower und std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Ausgabe: "Hello World From C++"
}
```

### Verwendung der Boost-Bibliothek:
Für fortgeschrittenere String-Manipulationen, einschließlich ortsabhängiger Kapitalisierung, möchten Sie vielleicht die Boost String Algo-Bibliothek verwenden.

Stellen Sie zunächst sicher, dass Sie die Boost-Bibliothek installiert und in Ihrem Projekt konfiguriert haben. Dann können Sie die notwendigen Header einbinden und deren Funktionen wie unten gezeigt verwenden.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // Ersten Buchstaben jedes Wortes großschreiben
    boost::algorithm::to_lower(capitalizedText); // stellt sicher, dass der String in Kleinbuchstaben ist
    capitalizedText[0] = std::toupper(capitalizedText[0]); // den ersten Buchstaben großschreiben

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // Großschreibung nach einem Leerzeichen
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Ausgabe: "Hello World From C++"
}
```

In diesem Fall vereinfacht Boost einige der Aufgaben zur String-Manipulation, erfordert jedoch immer noch einen benutzerdefinierten Ansatz für eine echte Kapitalisierung, da es hauptsächlich Transformations- und Fallumwandlungsdienste bietet.
