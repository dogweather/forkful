---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:38.046617-07:00
description: "YAML, som st\xE5r for YAML Ain't Markup Language, er et menneskelesbart\
  \ data serialiseringsformat. Programmerere bruker det for konfigurasjonsfiler,\u2026"
lastmod: '2024-03-13T22:44:41.120499-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for YAML Ain't Markup Language, er et menneskelesbart\
  \ data serialiseringsformat. Programmerere bruker det for konfigurasjonsfiler,\u2026"
title: Arbeider med YAML
weight: 41
---

## Hva & Hvorfor?

YAML, som står for YAML Ain't Markup Language, er et menneskelesbart data serialiseringsformat. Programmerere bruker det for konfigurasjonsfiler, datadumping, og lagring av hierarkisk data på grunn av dets lesbarhet og enkle-å-forstå syntaks sammenlignet med XML eller JSON.

## Hvordan:

For å jobbe med YAML i C++, er et populært valg `yaml-cpp`-biblioteket. Først, sørg for at du har `yaml-cpp` installert og korrekt koblet til ditt C++-prosjekt.

**Lese en YAML-fil:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Tittel: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Gitt en `config.yaml` som ser slik ut:

```yaml
title: "Example YAML"
```

Å kjøre den ovennevnte C++-koden vil produsere:

```
Tittel: Example YAML
```

**Skrive til en YAML-fil:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter ut;
    ut << YAML::BeginMap;
    ut << YAML::Key << "title" << YAML::Value << "Example YAML";
    ut << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << ut.c_str();
    
    return 0;
}
```

Denne koden vil lage en `output.yaml` med innholdet:

```yaml
title: Example YAML
```

Disse eksemplene tjener som en grunnleggende introduksjon til å lese fra og skrive til YAML-filer i C++ ved hjelp av `yaml-cpp`-biblioteket. For mer komplekse strukturer og brukstilfeller, utforsk `yaml-cpp`-dokumentasjonen for funksjoner som sekvenser, tagger, og mer avanserte serialiserings- og deserialiseringsteknikker.
