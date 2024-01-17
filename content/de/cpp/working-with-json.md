---
title:                "Arbeiten mit json"
html_title:           "C++: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?

Arbeiten mit JSON ist der Prozess des Einlesens, der Verarbeitung und des Schreibens von Daten im JSON-Format in C++. JSON steht für JavaScript Object Notation und ist ein häufig verwendetes Datenformat in der Webentwicklung. Programmierer nutzen JSON, um Daten zwischen Anwendungen auszutauschen oder um benutzerdefinierte Konfigurationen zu speichern.

# Wie geht's:

Um mit JSON in C++ zu arbeiten, müssen Sie zunächst die entsprechende Bibliothek hinzufügen. Eine häufig verwendete Bibliothek ist nlohmann/json, die einfach zu installieren und zu verwenden ist.

```
#include <iostream>
#include <fstream>
#include "json.hpp"

// JSON-Daten einlesen und ausgeben
using json = nlohmann::json;
json data = json::parse(std::ifstream("data.json"));
std::cout << data << std::endl;

// JSON-Daten schreiben
json newData = {{"name", "Max"}, {"age", 25}};
std::ofstream("newData.json") << newData;
```

Die Ausgabe des obigen Codes wäre:

```
{
  "name": "Max",
  "age": 25
}
```

# Tiefere Einblicke:

JSON wurde in den frühen 2000er Jahren als Alternative zu XML eingeführt und hat sich seitdem als beliebtes Datenformat in der Webentwicklung etabliert. Es ist leicht lesbar für Menschen und einfach zu verarbeiten für Computer. Alternativen zu JSON sind XML, YAML oder CSV, aber sie sind entweder komplexer oder weniger flexibel als JSON.

Die Implementierung von JSON in C++ kann mithilfe von verschiedenen Bibliotheken erfolgen, wie z.B. nlohmann/json, RapidJSON oder JsonCpp. Diese Bibliotheken bieten Funktionen zum Parsen, Bearbeiten und Schreiben von JSON-Daten.

# Siehe auch:

- [nlohmann/json](https://github.com/nlohmann/json) - Offizielle GitHub-Seite der nlohmann/json Bibliothek
- [RapidJSON](https://rapidjson.org) - Eine schnelle C++-Bibliothek zum Verarbeiten von JSON-Daten
- [JsonCpp](https://github.com/open-source-parsers/jsoncpp) - Eine weitere C++-Bibliothek für JSON-Verarbeitung