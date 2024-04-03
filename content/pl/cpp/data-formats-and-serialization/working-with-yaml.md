---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:49.006159-07:00
description: "Jak to zrobi\u0107: Pracuj\u0105c z YAML w C++, popularnym wyborem jest\
  \ biblioteka `yaml-cpp`. Najpierw upewnij si\u0119, \u017Ce masz zainstalowan\u0105\
  \ `yaml-cpp` i odpowiednio\u2026"
lastmod: '2024-03-13T22:44:35.733120-06:00'
model: gpt-4-0125-preview
summary: "Pracuj\u0105c z YAML w C++, popularnym wyborem jest biblioteka `yaml-cpp`."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Pracując z YAML w C++, popularnym wyborem jest biblioteka `yaml-cpp`. Najpierw upewnij się, że masz zainstalowaną `yaml-cpp` i odpowiednio połączoną z projektem C++.

**Czytanie pliku YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Tytuł: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Mając `config.yaml`, który wygląda tak:

```yaml
title: "Przykład YAML"
```

Uruchomienie powyższego kodu C++ wyprodukuje:

```
Tytuł: Przykład YAML
```

**Zapis do pliku YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Przykład YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Ten kod utworzy `output.yaml` z zawartością:

```yaml
title: Przykład YAML
```

Te przykłady służą jako podstawowe wprowadzenie do czytania z plików YAML i zapisywania do nich w C++ przy użyciu biblioteki `yaml-cpp`. Dla bardziej złożonych struktur i przypadków użycia, zgłęb dokumentację `yaml-cpp` w poszukiwaniu funkcji takich jak sekwencje, tagi oraz bardziej zaawansowane techniki serializacji i deserializacji.
