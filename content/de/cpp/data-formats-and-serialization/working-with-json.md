---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.250188-07:00
description: "Wie geht das: In C++ gibt es keine native Unterst\xFCtzung f\xFCr JSON,\
  \ aber Drittanbieter-Bibliotheken wie nlohmann/json machen es unkompliziert. So\
  \ verwenden\u2026"
lastmod: '2024-03-13T22:44:54.205681-06:00'
model: gpt-4-0125-preview
summary: "In C++ gibt es keine native Unterst\xFCtzung f\xFCr JSON, aber Drittanbieter-Bibliotheken\
  \ wie nlohmann/json machen es unkompliziert."
title: Arbeiten mit JSON
weight: 38
---

## Wie geht das:
In C++ gibt es keine native Unterstützung für JSON, aber Drittanbieter-Bibliotheken wie nlohmann/json machen es unkompliziert. So verwenden Sie es für grundlegende Aufgaben:

Zuerst stellen Sie sicher, dass Sie die Bibliothek installiert haben. Wenn Sie einen Paketmanager wie vcpkg oder Conan verwenden, können Sie `nlohmann/json` leicht zu Ihrem Projekt hinzufügen.

### JSON aus einem String parsen
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON-Daten als String
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // JSON-String parsen
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Auf Daten zugreifen
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Alter: " << jsonObject["age"] << "\n"
              << "Stadt: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Beispielausgabe:**

```
Name: John
Alter: 30
Stadt: New York
```

### JSON erzeugen
JSON-Daten zu erstellen, ist genauso unkompliziert; man weist einfach Werte einem `nlohmann::json`-Objekt zu.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Ein JSON-Objekt erstellen
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // JSON-Objekt in String konvertieren und ausgeben
    std::string jsonString = jsonObject.dump(4); // Argument 4 für hübsches Drucken
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Beispielausgabe:**

```
{
    "name": "Jane",
    "age": 25,
    "stadt": "Los Angeles"
}
```

Diese Beispiele demonstrieren die Kernfunktionalitäten für die Arbeit mit JSON in C++ unter Verwendung der `nlohmann/json`-Bibliothek. Mit diesen Grundlagen können Sie JSON für verschiedene Anwendungen parsen und generieren, von Konfigurationsdateien bis hin zum Datenaustausch in vernetzten Anwendungen.
