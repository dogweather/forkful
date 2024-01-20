---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON, kurz für JavaScript Object Notation, ist ein Datenformat zum Austausch von Daten. Programmierer nutzen JSON, weil es leicht lesbar und weit verbreitet ist, insbesondere für Web APIs.

## How to:
Um mit JSON in C++ zu arbeiten, kannst du Libraries wie `nlohmann::json` benutzen. Hier ist ein schnelles Beispiel:

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Erstellen eines JSON-Objekts
    nlohmann::json obj;
    obj["name"] = "Fritz";
    obj["age"] = 30;
    obj["is_programmer"] = true;

    // Convertieren des Objekts in einen String
    std::string json_string = obj.dump();
    std::cout << json_string << std::endl;

    // Lesen der Daten aus einem JSON-String
    auto parsed = nlohmann::json::parse(R"({"name": "Klaus", "hobbies": ["Schach", "Klettern"]})");
    std::cout << parsed["name"] << std::endl;

    return 0;
}
```

Ausgabe wäre:
```
{"age":30,"is_programmer":true,"name":"Fritz"}
Klaus
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre entwickelt und hat XML als bevorzugtes Format für den Datenverkehr im Web abgelöst. Alternativen zu JSON sind u.a. YAML und BSON, wobei JSON wegen seiner Einfachheit und Geschwindigkeit oft bevorzugt wird. Beim Arbeiten mit C++ und JSON ist wichtig zu wissen, dass die Standardbibliothek keine native Unterstützung bietet und deshalb Libraries wie `nlohmann::json` oder `jsoncpp` weit verbreitet sind. Diese Libraries vereinfachen das Parsen und Schreiben von JSON-Daten.

## See Also
- [nlohmann/json GitHub Repository](https://github.com/nlohmann/json)
- [JSON offizielle Website](https://www.json.org/json-de.html)