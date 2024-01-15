---
title:                "Arbeiten mit JSON"
html_title:           "C++: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand mit JSON arbeiten wollen? Ganz einfach: JSON ist eine strukturierte und leicht lesbare Datenformatierung, die es uns ermöglicht, Daten schnell und effizient auszutauschen und zu verarbeiten.

## Wie geht es

Um mit JSON in C++ zu arbeiten, müssen wir zunächst die Bibliothek *nlohmann/json* installieren. Dann können wir JSON-Objekte erstellen und bearbeiten, indem wir die richtigen Methoden und Operatoren verwenden.

```C++
#include <iostream>
#include <nlohmann/json.hpp>

using namespace std;
using json = nlohmann::json;

// JSON-Objekt erstellen
json my_obj = {
    {"name", "Max"},
    {"age", 25},
    {"is_senior", false}
};

// JSON-Objekt ausgeben
cout << my_obj << endl;

// Wert auslesen
cout << my_obj["name"] << endl; // Ausgabe: Max

// JSON-Objekt bearbeiten
my_obj["is_senior"] = true;
my_obj["hobbies"] = {"coding", "reading"};

cout << my_obj << endl;
```

Die Ausgabe des obigen Codes wäre:

```C++
{
    "name": "Max",
    "age": 25,
    "is_senior": false
}

Max

{
    "name": "Max",
    "age": 25,
    "is_senior": true,
    "hobbies": ["coding", "reading"]
}
```

## Tiefes Eintauchen

Die *nlohmann/json* Bibliothek bietet viele fortschrittliche Funktionen für die Arbeit mit JSON-Objekten, wie zum Beispiel das Zusammenführen von zwei Objekten, das Durchsuchen nach bestimmten Werten und das Serialisieren und Deserialisieren von Objekten in C++.

Einige nützliche Ressourcen für die Arbeit mit JSON in C++ sind:

- [nlohmann/json GitHub Repository](https://github.com/nlohmann/json)
- [JSON Tutorial auf cplusplus.com](https://www.cplusplus.com/doc/json/)
- [JSON Cheat Sheet für C++](https://github.com/zeeshanu/cpp-json-cheatsheet)

## Siehe auch

- [JSON in Python: Warum und Wie](https://link.to/german/python-json-article)
- [C++ Datenstrukturen: Vektoren, Listen und Maps](https://link.to/german/cpp-data-structures-article)