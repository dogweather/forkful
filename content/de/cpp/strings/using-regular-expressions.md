---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:08.242879-07:00
description: "Wie geht das: C++11 f\xFChrte Unterst\xFCtzung f\xFCr regul\xE4re Ausdr\xFC\
  cke in der Standardbibliothek `<regex>` ein und bietet ein robustes Framework f\xFC\
  r die Suche\u2026"
lastmod: '2024-03-13T22:44:54.174619-06:00'
model: gpt-4-0125-preview
summary: "C++11 f\xFChrte Unterst\xFCtzung f\xFCr regul\xE4re Ausdr\xFCcke in der\
  \ Standardbibliothek `<regex>` ein und bietet ein robustes Framework f\xFCr die\
  \ Suche und Manipulation von Zeichenketten."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie geht das:
C++11 führte Unterstützung für reguläre Ausdrücke in der Standardbibliothek `<regex>` ein und bietet ein robustes Framework für die Suche und Manipulation von Zeichenketten. Hier ein einfaches Beispiel für die Verwendung regulärer Ausdrücke zur Suche nach einem Muster innerhalb einer Zeichenkette:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string ziel = "Hallo, meine E-Mail ist beispiel@beispiel.com";
    std::regex email_muster(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(ziel, email_muster)) {
        std::cout << "E-Mail gefunden!" << std::endl;
    } else {
        std::cout << "Keine E-Mail gefunden." << std::endl;
    }

    return 0;
}
```
**Beispielausgabe**
```
E-Mail gefunden!
```

Für komplexere Manipulationen, wie das Ersetzen von Mustern innerhalb von Zeichenketten, können die regulären Ausdrücke von C++ sehr nützlich sein:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "Der Regen in Spanien fällt hauptsächlich auf die Ebene.";
    std::regex vokal_regex("([aeiou])");

    std::string ersetzter_text = std::regex_replace(text, vokal_regex, "*");
    std::cout << ersetzter_text << std::endl;

    return 0;
}
```
**Beispielausgabe**
```
D*r R*g*n *n Sp*n**n f*llt h*upts*chl*ch *uf d** Eb*n*.
```

Für Programmierer, die über die Standardbibliothek hinausgehen möchten, ist die Boost Regex-Bibliothek (`boost/regex.hpp`) eine beliebte Option von Drittanbietern, die verbesserte Regex-Fähigkeiten und Leistungsoptimierungen bietet, insbesondere für komplexe Muster oder umfangreiche Datenverarbeitung:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost-Bibliotheken machen Spaß!";
    boost::regex ausdruck("(\\w+)\\s(Bibliotheken)"); // Stimmt mit "Boost-Bibliotheken" überein
    std::string fmt("GNU \\1"); // Ersetzen durch "GNU Boost"

    std::string ergebnis = boost::regex_replace(s, ausdruck, fmt);
    std::cout << ergebnis << std::endl;

    return 0;
}
```
**Beispielausgabe**
```
GNU Boost machen Spaß!
```

Diese Beispiele kratzen nur an der Oberfläche der Fähigkeiten von C++ mit regulären Ausdrücken, sie illustrieren grundlegende Suchen, Musterabgleiche und Ersetzungen, entweder unter Verwendung der Standardbibliothek oder verstärkt durch die leistungsstarke Regex-Implementierung von Boost.
