---
title:                "Travailler avec yaml"
html_title:           "C++: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi utiliser YAML ?

YAML est un langage de serialization de données facile à lire et utilisé par les programmeurs pour stocker et échanger des informations. Cela permet de gérer les configurations et les données de manière structurée et portable.

## Comment faire :

Voici un exemple simple de code montrant comment lire un fichier YAML en C++ :

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main()
{
    YAML::Node node = YAML::LoadFile("config.yaml");

    std::cout << "Nom d'utilisateur: " << node["username"].as<std::string>() << "\n";
    std::cout << "Âge: " << node["age"].as<int>() << "\n";
    std::cout << "Langages de programmation: ";
    for (const auto& language : node["languages"])
    {
        std::cout << language.as<std::string>() << " ";
    }
    std::cout << "\n";

    return 0;
}
```

Voici le contenu du fichier `config.yaml` utilisé dans cet exemple :

```yaml
username: Jean
age: 25
languages:
  - C++
  - Python
  - JavaScript
```

Voici le résultat de l'exécution du code :

```
Nom d'utilisateur: Jean
Âge: 25
Langages de programmation: C++ Python JavaScript
```

## Plongée en profondeur :

- Contexte historique : YAML a été créé en 2001 par Clark Evans avec les contributions de nombreux autres développeurs.
- Alternatives : d'autres formats de données tels que JSON et XML peuvent également être utilisés pour stocker et échanger des informations structurées.
- Détails de mise en œuvre : YAML est basé sur la syntaxe du langage de programmation Python et utilise une indentation pour délimiter les différentes données.

## Voir aussi :

- [Site officiel de YAML](https://yaml.org/)
- [Documentation de la bibliothèque YAML-CPP](https://github.com/jbeder/yaml-cpp/wiki)