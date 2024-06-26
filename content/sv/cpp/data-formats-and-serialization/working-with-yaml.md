---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:39.236977-07:00
description: "Hur man g\xF6r: F\xF6r att arbeta med YAML i C++ \xE4r ett popul\xE4\
  rt val biblioteket `yaml-cpp`. Se f\xF6rst till att du har `yaml-cpp` installerat\
  \ och korrekt l\xE4nkat\u2026"
lastmod: '2024-03-13T22:44:38.229742-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att arbeta med YAML i C++ \xE4r ett popul\xE4rt val biblioteket `yaml-cpp`."
title: Att Arbeta med YAML
weight: 41
---

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
