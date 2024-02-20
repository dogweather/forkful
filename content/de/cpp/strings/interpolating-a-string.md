---
date: 2024-01-20 17:50:25.842274-07:00
description: "String-Interpolation erleichtert das Zusammensetzen von Strings, indem\
  \ Variablen oder Ausdr\xFCcke direkt in String-Literale eingebettet werden. Es macht\
  \ den\u2026"
lastmod: 2024-02-19 22:05:13.105606
model: gpt-4-1106-preview
summary: "String-Interpolation erleichtert das Zusammensetzen von Strings, indem Variablen\
  \ oder Ausdr\xFCcke direkt in String-Literale eingebettet werden. Es macht den\u2026"
title: Zeichenketten interpolieren
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation erleichtert das Zusammensetzen von Strings, indem Variablen oder Ausdrücke direkt in String-Literale eingebettet werden. Es macht den Code lesbarer und die String-Konstruktion effizienter.

## How to:
C++ hat keine eingebaute String-Interpolation wie einige andere Sprachen, aber wir können ähnliche Funktionalitäten mit `std::ostringstream` oder fmt-Bibliothek (C++20) erreichen:

```cpp
#include <iostream>
#include <sstream>
#include <fmt/core.h> // C++20 benötigt

int main() {
    int year = 2023;
    std::string event = "Kurs";

    // Mit std::ostringstream
    std::ostringstream oss;
    oss << "Willkommen im " << year << " " << event << "!";
    std::cout << oss.str() << std::endl;

    // Mit fmt-Bibliothek (C++20)
    std::string msg = fmt::format("Willkommen im {0} {1}!", year, event);
    std::cout << msg << std::endl;

    return 0;
}
```

Sample Output:
```
Willkommen im 2023 Kurs!
Willkommen im 2023 Kurs!
```

## Deep Dive
Historisch gesehen hatte C++ keine direkte Unterstützung für String-Interpolation, im Gegensatz zu Sprachen wie Python oder Ruby. Vor C++20 mussten Entwickler mit String-Streams (`std::stringstream`) oder manuellen String-Zusammenführungen arbeiten, was oft umständlich war.

Die `fmt`-Bibliothek, inkludiert in C++20, bringt Python-ähnliche String-Interpolation. Alternativ können auch externe Bibliotheken wie `boost::format` oder `fmtlib` vor C++20 verwendet werden.

Die Implementierung der String-Interpolation erfolgt üblicherweise durch das Parsen des String-Literals und das Ersetzen der Platzhalter mit den entsprechenden Variableninhalten. Die `fmt`-Bibliothek nutzt dabei ein kompaktes Format und bietet zusätzliche Formatierungsoptionen.

## See Also:
Hier sind einige Ressourcen, für tiefergehende Informationen:

- [fmtlib Dokumentation](https://fmt.dev/latest/index.html) – die fmt-Bibliothek für moderne C++-String-Interpolation.
- [C++20 Features](https://en.cppreference.com/w/cpp/20) – Informationen zu den Neuerungen in C++20.
- [std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream) – Dokumentation über string streams in C++.
