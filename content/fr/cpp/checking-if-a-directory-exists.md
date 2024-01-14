---
title:                "C++: Vérifier si un répertoire existe"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est parfois nécessaire de vérifier si un répertoire existe avant d'exécuter certaines opérations. Cela peut être utile lorsque l'on manipule des fichiers et des dossiers dynamiquement dans un programme C++. 

## Comment faire 

Voici comment vous pouvez vérifier si un répertoire existe en utilisant la bibliothèque standard de C++ : 

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main(){
    if(fs::exists("chemin/vers/votre/répertoire")){
        std::cout << "Le répertoire existe !" << std::endl;
    }
    else{
        std::cout << "Le répertoire n'existe pas." << std::endl;
    }

    return 0;
}
```

Output :
```
Le répertoire n'existe pas.
```

Vous pouvez également utiliser la fonction ```std::filesystem::is_directory()``` pour vérifier si un fichier est un répertoire ou non. Voici un exemple : 

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main(){
    if(fs::is_directory("chemin/vers/votre/fichier")){
        std::cout << "Le fichier est un répertoire !" << std::endl;
    }
    else{
        std::cout << "Le fichier n'est pas un répertoire." << std::endl;
    }

    return 0;
}
```

Output :
```
Le fichier n'est pas un répertoire.
```

## Profonde plongée 

Pour vérifier si un répertoire existe, la bibliothèque standard de C++ utilise la fonction système ```stat()```, qui retourne un objet de type ```struct stat``` contenant des informations sur le fichier ou le répertoire spécifié. En utilisant la fonction ```exists()```, nous vérifions simplement si le code de sortie de ```stat()``` est égal à zéro, ce qui signifie que le fichier ou le répertoire existe. 

## Voir aussi

- [Documentation sur la bibliothèque standard de C++ pour les fonctions de manipulation de fichiers et de dossiers](https://en.cppreference.com/w/cpp/filesystem)
- [Un tutoriel complet sur la manipulation de fichiers et de dossiers en C++](https://www.youtube.com/watch?v=IOkgBrXCtfo)