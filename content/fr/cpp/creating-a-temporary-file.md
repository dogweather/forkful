---
title:                "C++: Création d'un fichier temporaire"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en programmation C++

Si vous travaillez en programmation C++, vous avez peut-être entendu parler de la création de fichiers temporaires. Mais pourquoi créer un fichier temporaire en premier lieu ? La réponse est simple : pour stocker temporairement des données ou des informations nécessaires à votre programme, sans avoir à créer un fichier permanent qui prendrait de l'espace et serait difficile à gérer.

## Comment créer un fichier temporaire en C++

La création d'un fichier temporaire en C++ est facile grâce à la bibliothèque standard. Tout ce que vous avez à faire est d'inclure la bibliothèque `<fstream>` et d'utiliser la fonction `tmpfile()` pour créer un fichier temporaire vide. Ensuite, vous pouvez écrire des données dans le fichier temporaire à l'aide des fonctions d'E/S de fichier telles que `fprintf()` ou `fwrite()`. Voici un exemple de code :

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {

  FILE* tempFile = tmpfile();

  if (tempFile != NULL) {
    fprintf(tempFile, "Hello, world!");
    cout << "Données écrites avec succès dans le fichier temporaire." << endl;
    fclose(tempFile); // ferme le fichier temporaire
  } else {
    cout << "Impossible de créer le fichier temporaire." << endl;
  }

  return 0;
}
```

Le fichier temporaire créé sera automatiquement supprimé lorsque le programme se termine ou lorsque vous fermez le fichier à l'aide de `fclose()`. N'oubliez pas que vous pouvez également utiliser des fonctions telles que `fseek()` ou `fread()` pour gérer les données dans le fichier temporaire.

## Plongée en profondeur dans la création d'un fichier temporaire

Lorsqu'un fichier temporaire est créé, il est stocké dans le répertoire de travail actuel du programme. Il est également attribué un nom unique pour éviter les conflits avec d'autres fichiers temporaires. Vous pouvez obtenir ce nom en utilisant la fonction `tmpnam()` et même l'utiliser pour créer un fichier permanent si nécessaire.

De plus, la fonction `tmpfile()` crée un fichier dans le mode binaire, ce qui signifie que les informations écrites dans le fichier ne seront pas converties ou traitées en fonction du système d'exploitation. Cela peut être utile si vous avez besoin de stocker des données de manière précise et fiable.

## Voir aussi

- [Documentation de la bibliothèque `<fstream>` en C++](https://www.cplusplus.com/reference/fstream/)
- [Tutoriel sur la gestion des fichiers en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Exemple de code pour la création de fichiers temporaires en C++](https://www.geeksforgeeks.org/creating-temporary-file-names-in-c-programming/)

En utilisant la fonction `tmpfile()` en conjonction avec d'autres fonctions d'E/S de fichier, vous pouvez facilement créer et gérer des fichiers temporaires dans vos programmes C++. Alors n'hésitez pas à utiliser cette fonction pratique pour faciliter votre développement !