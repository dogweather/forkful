---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:52.803757-07:00
description: "Comment faire : Dans le C++ moderne (C++17 et au-del\xE0), vous pouvez\
  \ utiliser la biblioth\xE8que filesystem pour v\xE9rifier si un r\xE9pertoire existe.\
  \ Elle offre\u2026"
lastmod: '2024-03-13T22:44:58.179340-06:00'
model: gpt-4-0125-preview
summary: "Dans le C++ moderne (C++17 et au-del\xE0), vous pouvez utiliser la biblioth\xE8\
  que filesystem pour v\xE9rifier si un r\xE9pertoire existe."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
Dans le C++ moderne (C++17 et au-delà), vous pouvez utiliser la bibliothèque filesystem pour vérifier si un répertoire existe. Elle offre une manière simple et standardisée d’effectuer des opérations sur le système de fichiers, y compris la vérification de l'existence d'un répertoire.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/chemin/vers/repertoire";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Le répertoire existe." << std::endl;
    } else {
        std::cout << "Le répertoire n'existe pas." << std::endl;
    }

    return 0;
}
```
Sortie d'exemple si le répertoire existe :
```
Le répertoire existe.
```

Sortie d'exemple si le répertoire n'existe pas :
```
Le répertoire n'existe pas.
```

Pour les projets qui n'utilisent pas encore C++17 ou pour des fonctionnalités supplémentaires, la bibliothèque Boost Filesystem est un choix tiers populaire qui offre une fonctionnalité similaire.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/chemin/vers/repertoire";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Le répertoire existe." << std::endl;
    } else {
        std::cout << "Le répertoire n'existe pas." << std::endl;
    }

    return 0;
}
```
En utilisant Boost Filesystem, la sortie serait identique à l'exemple du système de fichiers C++17, en fonction de l'existence du répertoire au chemin spécifié.
