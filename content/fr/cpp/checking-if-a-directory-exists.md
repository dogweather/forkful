---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? 
(Quoi et Pourquoi ?)

Parfois, on doit vérifier si un dossier existe pour éviter les erreurs lors de la manipulation de fichiers. Cela aide à la robustesse de nos programmes.

## How to:
(Comment faire:)

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path pth{"./chemin/vers/dossier"};
    
    if(fs::exists(pth)) {
        std::cout << "Le dossier existe!" << std::endl;
    } else {
        std::cout << "Le dossier n'existe pas!" << std::endl;
    }
    
    return 0;
}

```
Sortie échantillon:
```
Le dossier existe!
```
ou
```
Le dossier n'existe pas!
```

## Deep Dive
(Plongée en profondeur)

Avant C++17, on utilisait `struct stat` avec `stat()` ou des fonctions propres à chaque système d'exploitation. Maintenant, `std::filesystem` simplifie la portabilité. C'est plus sûr et plus lisible. Il existe aussi des bibliothèques tierces comme Boost.Filesystem, mais `std::filesystem` devient la norme.

## See Also
(Voir aussi)

- Documentation de `std::filesystem`: https://en.cppreference.com/w/cpp/filesystem
- Tutoriel Boost.Filesystem: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
- Comparaison entre différentes méthodes: https://stackoverflow.com/questions/18100097/portable-way-to-check-if-directory-exists-windows-linux-c
