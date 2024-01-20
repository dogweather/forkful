---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilzeichenketten (Substrings) ist der Prozess, einen Teil einer Zeichenkette zu entfernen und separat zu verwenden. Programmierer tun dies, um Text und Daten effizient zu manipulieren und zu analysieren.

## Wie geht das:

Ein grundlegender Weg, um Substrings in C++ zu extrahieren, ist die Verwendung der `substr` Funktion, die in der `std::string` Klasse definiert ist. 

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Hallo, Welt!";
    std::string sub = s.substr(0, 5);

    std::cout << sub << std::endl;  // Gibt "Hallo" aus

    return 0;
}
```
## Tiefer Einblick

Historisch gesehen wurde das Extrahieren von Substrings in C++ mittels Zeiger und Dynamischen Speichers durchgeführt. Doch mit der Einführung der `std::string` Klasse in der Standardbibliothek wurde die Aufgabe viel einfacher.

Zu den Alternativen gehört der Gebrauch von Regular Expressions oder `std::stringstream`, die jedoch komplizierter und overheadlastig sein können. Die `substr` Funktion hingegen bietet eine direkte und effiziente Methode zur Extraktion von Substrings.

Was die Implementierungsdetails betrifft, so gibt `substr` eine Kopie des Substrings zurück, nicht einen Zeiger oder eine Referenz, was bedeutet, dass die ursprüngliche Zeichenkette unverändert bleibt.

## Siehe Auch

Für weitere Details und verwandte Themen, siehe:

- [Cplusplus.com - string::substr](http://www.cplusplus.com/reference/string/string/substr/)
- [Stackoverflow - Wie Substrings in C++ extrahieren](https://stackoverflow.com/questions/14265581/parse-split-a-string-in-c-using-a-delimiter)
- [Das komplette C++ Tutorial - Strings und ihre Funktionen](https://www.learncpp.com/cpp-tutorial/4-8a-an-introduction-to-stdstring/)