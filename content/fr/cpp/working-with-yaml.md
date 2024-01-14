---
title:                "C++: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur C++ à la recherche d'un moyen simple et efficace pour stocker et transférer des données, alors travailler avec YAML peut être la solution parfaite pour vous. YAML est un format de sérialisation de données qui est facile à lire et à écrire pour les humains, ce qui en fait un choix populaire pour les fichiers de configuration et les échanges de données.

## Comment faire

Pour travailler avec YAML en C++, vous devez utiliser une bibliothèque comme yaml-cpp. Voici un exemple de code pour lire et écrire dans un fichier YAML en utilisant cette bibliothèque :

```C++
#include <iostream>
#include <fstream>
#include "yaml-cpp/yaml.h"

int main() {
// Écrire dans un fichier YAML
YAML::Emitter out;
out << YAML::BeginMap;
out << YAML::KeyValue("name", "John");
out << YAML::KeyValue("age", 25);
out << YAML::KeyValue("hobbies", YAML::BeginSeq) << "Hiking" << "Reading" << "Cooking" << YAML::EndSeq;
out << YAML::EndMap;

std::ofstream fout("info.yaml");
fout << out.c_str();
fout.close();

// Lire à partir du fichier YAML
YAML::Node node = YAML::LoadFile("info.yaml");
std::cout << node["name"].as<std::string>() << std::endl;
std::cout << node["age"].as<int>() << std::endl;
YAML::Node hobbies = node["hobbies"];
for (auto hobby : hobbies) {
std::cout << hobby.as<std::string>() << std::endl;
}

return 0;
}
```

Voici le contenu du fichier YAML qui sera créé :

```YAML
name: John
age: 25
hobbies:
- Hiking
- Reading
- Cooking
```

## Plongée en profondeur

En plus des fonctionnalités de base pour lire et écrire dans des fichiers YAML, yaml-cpp offre également une fonctionnalité intéressante appelée le "Node API". Cela vous permet de manipuler des données YAML en tant qu'objet et d'accéder aux valeurs de manière plus pratique. Voici un exemple d'utilisation de cette fonctionnalité :

```C++
YAML::Node node = YAML::LoadFile("info.yaml");
std::string name = node["name"].as<std::string>();
int age = node["age"].as<int>();
node["hobbies"].push_back("Gardening"); // Ajoute un nouveau hobby
node["age"] = age + 1; // Met à jour l'âge de la personne
```

En utilisant le "Node API", vous pouvez également facilement vérifier la présence d'une clé dans un nœud YAML et gérer les erreurs lors de la conversion de valeurs.

## Voir aussi

Pour en savoir plus sur YAML et sur la bibliothèque yaml-cpp, vous pouvez consulter les liens suivants :

- Documentation officielle de YAML : https://yaml.org/
- Documentation de yaml-cpp : https://github.com/jbeder/yaml-cpp/wiki/Tutorial
- Exemple de projet utilisant yaml-cpp : https://github.com/jbeder/yaml-cpp/tree/master/examples