---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:01.272690-07:00
description: "Hur man g\xF6r: I C++ finns inget inbyggt st\xF6d f\xF6r JSON, men tredjepartsbibliotek\
  \ som nlohmann/json g\xF6r det enkelt. S\xE5 h\xE4r anv\xE4nder du det f\xF6r grundl\xE4\
  ggande\u2026"
lastmod: '2024-03-13T22:44:38.230780-06:00'
model: gpt-4-0125-preview
summary: "I C++ finns inget inbyggt st\xF6d f\xF6r JSON, men tredjepartsbibliotek\
  \ som nlohmann/json g\xF6r det enkelt."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:
I C++ finns inget inbyggt stöd för JSON, men tredjepartsbibliotek som nlohmann/json gör det enkelt. Så här använder du det för grundläggande uppgifter:

Först, se till att du har biblioteket installerat. Om du använder en pakethanterare som vcpkg eller Conan kan du enkelt lägga till `nlohmann/json` i ditt projekt.

### Tolkning av JSON från en sträng
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON-data som en sträng
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Tolka JSON-sträng
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Tillgång till data
    std::cout << "Namn: " << jsonObject["name"] << "\n"
              << "Ålder: " << jsonObject["age"] << "\n"
              << "Stad: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Exempelutskrift:**

```
Namn: John
Ålder: 30
Stad: New York
```

### Generera JSON
Att skapa JSON-data är lika enkelt; du tilldelar helt enkelt värden till ett `nlohmann::json`-objekt.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Skapa ett JSON-objekt
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Konvertera JSON-objekt till sträng och skriv ut
    std::string jsonString = jsonObject.dump(4); // Argument 4 för snygg utskrift
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Exempelutskrift:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Dessa exempel demonstrerar grundläggande funktionalitet för att arbeta med JSON i C++ med användning av `nlohmann/json`-biblioteket. Med dessa grunder kan du tolka och generera JSON för olika tillämpningar, från konfigurationsfiler till datatransport i nätverksanslutna applikationer.
