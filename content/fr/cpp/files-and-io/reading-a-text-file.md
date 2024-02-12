---
title:                "Lecture d'un fichier texte"
aliases:
- /fr/cpp/reading-a-text-file.md
date:                  2024-01-20T17:53:58.105345-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Lire un fichier texte en C++, c'est accéder au contenu d'un fichier stocké sur votre disque dur, ligne par ligne ou en bloc. Les programmeurs le font pour manipuler des données, configurer des applications ou tout simplement charger du texte.

## Comment faire :

```cpp
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream fichier("exemple.txt");
    std::string ligne;
    
    if(fichier.is_open()) {
        while(getline(fichier, ligne)) {
            std::cout << ligne << '\n';
        }
        fichier.close();
    } else {
        std::cout << "Impossible d'ouvrir le fichier." << std::endl;
    }
    
    return 0;
}
```
Sortie attendue (dépend du contenu de `exemple.txt`):
```
Première ligne du fichier texte
Deuxième ligne du fichier texte
...
```

## Exploration approfondie

Historiquement, lire des fichiers en C++ était plus compliqué avec l’utilisation des flux de fichiers C. Avec l'introduction de la bibliothèque standard C++, l'accès aux fichiers est devenu plus simple grâce à `std::ifstream` pour la lecture, `std::ofstream` pour l'écriture, et `std::fstream` pour les deux.

Si `std::ifstream` est parfait pour la plupart des cas d'utilisation, il existe des alternatives comme le mappage de fichiers en mémoire avec `mmap` sur Unix ou `CreateFileMapping` sur Windows, qui peuvent être plus performants pour les gros fichiers.

Quand à l'implémentation, il est important de bien gérer l'ouverture et la fermeture des fichiers pour éviter des fuites de mémoire. Utiliser la RAII (Resource Acquisition Is Initialization) en C++ avec des gestionnaires de ressources peut automatiquement fermer les fichiers, même en cas d'erreur.

## À voir également

- CPPReference - Entrée/Sortie de flux de fichiers : https://en.cppreference.com/w/cpp/io
- OpenClassrooms - C++ : Lire et écrire dans des fichiers : https://openclassrooms.com/fr/courses/1894236-programmez-avec-le-langage-c/1894377-lire-et-ecrire-dans-des-fichiers
- Stack Overflow - Lire un fichier ligne par ligne en C++ : https://stackoverflow.com/questions/7868936/read-file-line-by-line-using-ifstream-in-c
