---
date: 2024-01-20 17:47:14.361246-07:00
description: "So geht\u2019s: ."
lastmod: '2024-03-13T22:44:54.175640-06:00'
model: gpt-4-1106-preview
summary: .
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## So geht’s:
```C++
#include <iostream>
#include <string>

int main() {
    std::string meinString = "Hallo, Welt!";
    std::cout << "Länge: " << meinString.length() << '\n'; // Ausgabe: Länge: 12

    // C-Style String
    const char* cString = "Hallo, C!";
    std::cout << "Länge: " << strlen(cString) << '\n'; // Ausgabe: Länge: 9

    return 0;
}
```

## Tiefere Einblicke:
Früher in C, vor C++, verwendeten Entwickler 'char' Arrays und die Funktion `strlen` aus der Standardbibliothek `<cstring>`, um die Länge eines Strings zu finden. C++ bietet mit `std::string` eine Klasse, die viele Operationen sicherer und einfacher macht, einschließlich der Bestimmung der Länge mittels `length()` oder `size()` Methoden.

Alternativen: In manchen Fällen kann es nützlich sein, die Länge eines Strings manuell zu bestimmen, insbesondere in einer nicht standardisierten Umgebung. Hierfür würde man durch die Zeichen iterieren, bis das Nullzeichen '\0' gefunden wird.

Implementierungsdetails: `std::string::length()` gibt einen `size_t` Wert zurück, der die Anzahl der Zeichen im String angibt, ohne das abschließende Nullzeichen zu zählen. Intern hält ein `std::string` Objekt diese Information, sodass die Abfrage der Länge konstante Zeit kostet (O(1)), im Gegensatz zum manuellen Durchgehen des Strings.

## Siehe auch:
- C++ Standardbibliothek `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- C-Standardbibliothek `cstring` (für `strlen` und andere C-String-Operationen): https://en.cppreference.com/w/c/string/byte
- C++ Referenz für `std::string::length()`: https://en.cppreference.com/w/cpp/string/basic_string/length
