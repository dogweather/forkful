---
title:                "Att Arbeta med YAML"
aliases:
- /sv/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:39.236977-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för YAML Ain't Markup Language, är ett läsbart data-serialiseringsformat för människor. Programmerare använder det för konfigurationsfiler, data dumpning och lagring av hierarkisk data på grund av dess läsbarhet och lättförståeliga syntax jämfört med XML eller JSON.

## Hur man gör:

För att arbeta med YAML i C++ är ett populärt val biblioteket `yaml-cpp`. Se först till att du har `yaml-cpp` installerat och korrekt länkat till ditt C++-projekt.

**Läsa en YAML-fil:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Titel: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Givet en `config.yaml` som ser ut såhär:

```yaml
title: "Exempel YAML"
```

Att köra ovanstående C++-kod skulle producera:

```
Titel: Exempel YAML
```

**Skriva till en YAML-fil:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter ut;
    ut << YAML::BeginMap;
    ut << YAML::Key << "title" << YAML::Value << "Exempel YAML";
    ut << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << ut.c_str();
    
    return 0;
}
```

Denna kod kommer att skapa en `output.yaml` med innehållet:

```yaml
title: Exempel YAML
```

Dessa exempel fungerar som en grundläggande introduktion till att läsa från och skriva till YAML-filer i C++ med användning av `yaml-cpp`-biblioteket. För mer komplexa strukturer och användningsfall, utforska `yaml-cpp`-dokumentationen för funktioner som sekvenser, taggar och mer avancerade serialiserings- och deserialiseringstekniker.
