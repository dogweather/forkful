---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:38.781906-07:00
description: "Miten: YAML:n k\xE4sittelyyn C++:ssa suosittu valinta on `yaml-cpp`\
  \ kirjasto. Varmista ensin, ett\xE4 sinulla on `yaml-cpp` asennettuna ja oikein\
  \ linkitettyn\xE4\u2026"
lastmod: '2024-03-13T22:44:56.885754-06:00'
model: gpt-4-0125-preview
summary: "YAML:n k\xE4sittelyyn C++:ssa suosittu valinta on `yaml-cpp` kirjasto."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Miten:
YAML:n käsittelyyn C++:ssa suosittu valinta on `yaml-cpp` kirjasto. Varmista ensin, että sinulla on `yaml-cpp` asennettuna ja oikein linkitettynä C++ projektiisi.

**YAML-tiedoston lukeminen:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Otsikko: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Olettaen, että `config.yaml` näyttää tältä:

```yaml
title: "Esimerkki YAML"
```

Yllä olevan C++ koodin suorittaminen tuottaisi:

```
Otsikko: Esimerkki YAML
```

**Kirjoittaminen YAML-tiedostoon:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Esimerkki YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Tämä koodi luo `output.yaml` tiedoston sisällöllä:

```yaml
title: Esimerkki YAML
```

Nämä esimerkit toimivat perustietoina YAML-tiedostojen lukemisesta ja kirjoittamisesta C++:ssa käyttäen `yaml-cpp` kirjastoa. Tutki `yaml-cpp` dokumentaatiota monimutkaisempia rakenteita ja käyttötarkoituksia varten, kuten sekvenssejä, tageja ja edistyneempiä sarjallistamis- ja deserialisointitekniikoita varten.
