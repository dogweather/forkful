---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:54.271567-07:00
description: "Hvordan: I C++ er det ingen innebygd st\xF8tte for JSON, men tredjepartsbiblioteker\
  \ som nlohmann/json gj\xF8r det enkelt. Slik bruker du det for grunnleggende\u2026"
lastmod: '2024-03-13T22:44:41.121673-06:00'
model: gpt-4-0125-preview
summary: "I C++ er det ingen innebygd st\xF8tte for JSON, men tredjepartsbiblioteker\
  \ som nlohmann/json gj\xF8r det enkelt."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
I C++ er det ingen innebygd støtte for JSON, men tredjepartsbiblioteker som nlohmann/json gjør det enkelt. Slik bruker du det for grunnleggende oppgaver:

Først, sørg for at du har biblioteket installert. Hvis du bruker en pakkebehandler som vcpkg eller Conan, kan du enkelt legge til `nlohmann/json` til prosjektet ditt.

### Parse JSON fra en streng
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON-data som en streng
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Parse JSON-strengen
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Tilgang til data
    std::cout << "Navn: " << jsonObject["name"] << "\n"
              << "Alder: " << jsonObject["age"] << "\n"
              << "By: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Eksempel på utdata:**

```
Navn: John
Alder: 30
By: New York
```

### Generere JSON
Å opprette JSON-data er like rett frem; du tildeler rett og slett verdier til et `nlohmann::json`-objekt.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Opprette et JSON-objekt
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Konvertere JSON-objekt til streng og skrive ut
    std::string jsonString = jsonObject.dump(4); // Argument 4 for pen utskrift
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Eksempel på utdata:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Disse eksemplene demonstrerer kjernefunksjonalitet for å arbeide med JSON i C++ ved bruk av `nlohmann/json`-biblioteket. Med disse grunnleggende prinsippene kan du parse og generere JSON for ulike applikasjoner, fra konfigurasjonsfiler til datautveksling i nettverksapplikasjoner.
