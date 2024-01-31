---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Mit Regular Expressions (RegEx) suchen wir nach Mustern in Texten. Programmierer nutzen sie, weil sie mächtig und effizient Muster erkennen und manipulieren können.

## Anleitung:
```C++
#include <iostream>
#include <regex>
int main() {
    std::string text = "C++ ist cool, nicht wahr?";
    std::regex re("cool");
    bool match = std::regex_search(text, re);
    std::cout << (match ? "Muster gefunden!" : "Muster nicht gefunden.") << '\n';
    return 0;
}
```
Ausgabe:
```
Muster gefunden!
```

## Tiefgang:
Regular Expressions haben ihre Wurzeln in theoretischer Informatik und formaler Sprachtheorie. Alternativen zu RegEx sind Parser oder String-Suche-Algorithmen, aber sie sind oft weniger flexibel. In C++ implementieren wir Regular Expressions mit der `<regex>`-Bibliothek, die seit C++11 Standard ist.

## Siehe Auch:
- C++ Standard Library: https://en.cppreference.com/w/cpp/header/regex
- Regular Expressions Grundlagen: https://www.regular-expressions.info/
- Boost.Regex (eine leistungsstarke RegEx Bibliothek vor C++11): https://www.boost.org/doc/libs/release/libs/regex/
