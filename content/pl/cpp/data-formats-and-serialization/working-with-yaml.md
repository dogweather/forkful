---
title:                "Praca z YAML"
aliases: - /pl/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:49.006159-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, co oznacza YAML Ain't Markup Language, jest czytelnym dla człowieka formatem serializacji danych. Programiści używają go do plików konfiguracyjnych, zrzutów danych i przechowywania hierarchicznych danych ze względu na jego czytelność i łatwość zrozumienia składni w porównaniu do XML lub JSON.

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
