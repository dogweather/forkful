---
title:                "Travailler avec JSON"
aliases:
- fr/cpp/working-with-json.md
date:                  2024-02-03T19:21:54.690064-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

JSON (JavaScript Object Notation) est un format léger pour stocker et transporter des données, ce qui en fait un excellent moyen d'échange de données entre les serveurs et les applications web. Les programmeurs utilisent JSON en raison de sa facilité de lecture par les humains et de sa simplicité d'analyse par les machines, notamment lorsqu'ils travaillent sur des applications nécessitant un échange de données sur Internet ou des paramètres de configuration.

## Comment faire :

En C++, il n'y a pas de support natif pour JSON, mais des bibliothèques tierces comme nlohmann/json rendent cela simple. Voici comment l'utiliser pour des tâches basiques :

Premièrement, assurez-vous que vous avez la bibliothèque installée. Si vous utilisez un gestionnaire de packages comme vcpkg ou Conan, vous pouvez facilement ajouter `nlohmann/json` à votre projet.

### Analyser du JSON à partir d'une chaîne de caractères

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Données JSON sous forme de chaîne de caractères
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Analyser la chaîne JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Accéder aux données
    std::cout << "Nom : " << jsonObject["name"] << "\n"
              << "Âge : " << jsonObject["age"] << "\n"
              << "Ville : " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Sortie d'exemple :**

```
Nom : John
Âge : 30
Ville : New York
```

### Générer du JSON

Créer des données JSON est tout aussi simple ; vous assignez simplement des valeurs à un objet `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Création d'un objet JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Convertir l'objet JSON en chaîne de caractères et imprimer
    std::string jsonString = jsonObject.dump(4); // Argument 4 pour l'impression formatée
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Sortie d'exemple :**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Ces exemples démontrent les fonctionnalités de base pour travailler avec JSON en C++ en utilisant la bibliothèque `nlohmann/json`. Avec ces bases, vous pouvez analyser et générer du JSON pour diverses applications, des fichiers de configuration à l'échange de données dans des applications en réseau.
