---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Le YAML, c'est de la data en texte simple. Les devs l'utilisent pour la config ou des données qui bougent pas mal. C'est facile à lire et à écrire pour les humains et les machines.

## How to:
On va utiliser la lib `yaml-cpp`. Installe avec `vcpkg install yaml-cpp` ou ton gestionnaire de paquets.

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // Création d'un YAML node
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "name" << YAML::Value << "Gérard";
    out << YAML::Key << "job" << YAML::Value << "Developer";
    out << YAML::EndMap;

    // Écriture dans un fichier
    std::ofstream fout("config.yaml");
    fout << out.c_str();

    // Chargement depuis le fichier
    YAML::Node config = YAML::LoadFile("config.yaml");

    if (config["name"]) {
        std::cout << "Bonjour, " << config["name"].as<std::string>() << "!";
    }
    
    return 0;
}
```

Ça sort :
```
Bonjour, Gérard!
```

## Deep Dive
YAML date de 2001. Développé parce que XML c'était trop dense et compliqué. Les alternatives incluent JSON et TOML. YAML est super pour des trucs complexes car il gère les relations entre données. yaml-cpp est efficace et riche en fonctionnalités, mais attention à la sécurité quand tu charges des YAML depuis des sources non fiables.

## See Also
- La doc officielle YAML : https://yaml.org
- yaml-cpp GitHub : https://github.com/jbeder/yaml-cpp
- Comparaison de JSON et YAML : https://phoenixnap.com/kb/yaml-vs-json