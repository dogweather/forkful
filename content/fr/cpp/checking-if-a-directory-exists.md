---
title:                "C++: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi
Tout développeur a probablement rencontré cette situation : vous voulez accéder à un dossier spécifique dans votre programme mais vous n'êtes pas sûr de son existence. C'est là qu'entre en jeu la vérification de l'existence d'un répertoire.

## Comment faire 
Pour vérifier si un répertoire existe dans votre programme C++, vous pouvez utiliser la fonction ```std::filesystem::exists(path)```. Cette fonction vérifie si le chemin spécifié en tant que paramètre existe ou non. Si le chemin est un répertoire valide, la fonction renvoie ```true```, sinon elle renvoie ```false```.

```C++
#include <iostream>
#include <filesystem>

using namespace std;

int main() {

    // Définir le chemin du répertoire
    string chemin = "/home/user/Documents";

    // Vérifier l'existence du répertoire
    bool exist = std::filesystem::exists(chemin);

    // Afficher la sortie
    if (exist) {
        cout << "Le répertoire existe." << endl;
    } else {
        cout << "Le répertoire n'existe pas." << endl;
    }

    return 0;
}
```

Output:
```
Le répertoire existe.
```

## Plongée en profondeur
La fonction ```std::filesystem::exists(path)``` utilise la classe ```path``` de la bibliothèque standard pour représenter les chemins de fichiers et de répertoires. Il est important de noter que cette fonction ne fait que vérifier l'existence d'un chemin et ne garantit pas qu'il s'agisse d'un répertoire valide. Elle peut également renvoyer ```true``` pour des chemins qui pointent vers un fichier au lieu d'un répertoire.

Il existe également d'autres fonctions dans la bibliothèque standard C++ pour manipuler les fichiers et les répertoires, telles que ```std::filesystem::create_directory()``` pour créer un nouveau répertoire et ```std::filesystem::remove()``` pour supprimer un fichier ou un répertoire.

## Voir aussi
- [Documentation C++ sur la gestion des fichiers et des répertoires](https://en.cppreference.com/w/cpp/filesystem)
- [Tutorial sur l'utilisation de la bibliothèque standard C++ pour manipuler les fichiers et les répertoires](https://www.learncpp.com/cpp-tutorial/17-introduction-to-the-file-system/)
- [Article sur les bonnes pratiques de manipulation des répertoires en C++](https://www.fluentcpp.com/2017/04/21/how-to-split-a-path-into-directory-names-files-names-and-extension-with-stdfilesystem/)