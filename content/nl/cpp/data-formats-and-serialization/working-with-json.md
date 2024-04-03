---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.067395-07:00
description: 'Hoe te: Om met JSON in C++ te werken, moet je een bibliotheek zoals
  `nlohmann/json` gebruiken. Hier is hoe je JSON-gegevens kunt parsen en genereren.'
lastmod: '2024-03-13T22:44:51.134564-06:00'
model: gpt-4-0125-preview
summary: Om met JSON in C++ te werken, moet je een bibliotheek zoals `nlohmann/json`
  gebruiken.
title: Werken met JSON
weight: 38
---

## Hoe te:
Om met JSON in C++ te werken, moet je een bibliotheek zoals `nlohmann/json` gebruiken. Hier is hoe je JSON-gegevens kunt parsen en genereren:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON parsen
    std::string str = R"({"name":"John", "age":30, "city":"New York"})";
    nlohmann::json geparsed = nlohmann::json::parse(str);

    // Elementen benaderen
    std::cout << "Naam: " << geparsed["name"] << std::endl;
    std::cout << "Leeftijd: " << geparsed["age"] << std::endl;

    // JSON genereren
    nlohmann::json j;
    j["name"] = "Jane";
    j["age"] = 25;
    j["city"] = "Los Angeles";

    std::cout << "Gegenereerde JSON: " << j.dump(4) << std::endl;

    return 0;
}
```

Voorbeelduitvoer:
```
Naam: John
Leeftijd: 30
Gegenereerde JSON: {
    "age": 25,
    "city": "Los Angeles",
    "name": "Jane"
}
```

## Diepgaande Duik:
JSON werd geïntroduceerd als een eenvoudig tekstformaat voor gegevensuitwisseling en werd door zijn eenvoud en brede adoptatie een standaard. Alternatieven zoals XML bestaan, maar JSON leidt in web-API's vanwege zijn lagere wijdlopigheid en betere leesbaarheid. C++ heeft geen native ondersteuning voor JSON, vandaar dat bibliotheken zoals `nlohmann/json` populair zijn voor het afhandelen van serialisatie en deserialisatie, met een schone API die het werken met native datatypes nabootst.

## Zie Ook:
- GitHub-repository voor `nlohmann/json`: https://github.com/nlohmann/json
- Officiële JSON-website voor meer over het formaat: https://www.json.org/json-en.html
- Voor XML-afhandeling in C++: https://pugixml.org/
- Cppreference-pagina over string streams voor geavanceerde stringafhandeling in C++: https://en.cppreference.com/w/cpp/io/basic_stringstream
