---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:01.272690-07:00
description: "JSON (JavaScript Object Notation) \xE4r ett l\xE4tt format f\xF6r att\
  \ lagra och transportera data, vilket g\xF6r det till ett utm\xE4rkt medium f\xF6\
  r datatransport mellan\u2026"
lastmod: 2024-02-19 22:04:57.469047
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \xE4r ett l\xE4tt format f\xF6r att lagra\
  \ och transportera data, vilket g\xF6r det till ett utm\xE4rkt medium f\xF6r datatransport\
  \ mellan\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad och varför?

JSON (JavaScript Object Notation) är ett lätt format för att lagra och transportera data, vilket gör det till ett utmärkt medium för datatransport mellan servrar och webbapplikationer. Programmerare använder JSON på grund av dess lättlästa format för människor och enkla tolkbarhet av maskiner, särskilt när de arbetar med applikationer som kräver datatransport över internet eller konfigurationsinställningar.

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
