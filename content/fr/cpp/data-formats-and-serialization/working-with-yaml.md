---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:50.238706-07:00
description: "Comment faire : Pour travailler avec YAML en C++, un choix populaire\
  \ est la biblioth\xE8que `yaml-cpp`. Assurez-vous d'abord que vous avez install\xE9\
  \ `yaml-\u2026"
lastmod: '2024-03-13T22:44:58.190006-06:00'
model: gpt-4-0125-preview
summary: "Pour travailler avec YAML en C++, un choix populaire est la biblioth\xE8\
  que `yaml-cpp`."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Pour travailler avec YAML en C++, un choix populaire est la bibliothèque `yaml-cpp`. Assurez-vous d'abord que vous avez installé `yaml-cpp` et qu'il est correctement lié à votre projet C++.

**Lire un fichier YAML :**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Titre : " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Étant donné un `config.yaml` qui ressemble à ceci :

```yaml
title: "Exemple YAML"
```

L'exécution du code C++ ci-dessus produirait :

```
Titre : Exemple YAML
```

**Écrire dans un fichier YAML :**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Exemple YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Ce code créera un `output.yaml` avec le contenu :

```yaml
title: Exemple YAML
```

Ces exemples servent d'introduction de base à la lecture et à l'écriture de fichiers YAML en C++ en utilisant la bibliothèque `yaml-cpp`. Pour des structures plus complexes et des cas d'utilisation, explorez la documentation de `yaml-cpp` pour des fonctionnalités telles que les séquences, les tags, et des techniques de sérialisation et désérialisation plus avancées.
