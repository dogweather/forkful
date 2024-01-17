---
title:                "Vérifier si un répertoire existe"
html_title:           "C++: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le vérifier?

 Vérifier si un répertoire existe est une opération courante en programmation qui permet de s'assurer qu'un dossier donné existe avant de procéder à d'autres actions. Cela peut être utile lors de la création ou de la manipulation de fichiers dans un programme.

# Comment faire:

Voici un exemple de code en C++ montrant comment vérifier si un dossier existe:

```
#include <iostream>
#include <filesystem> // inclure la bibliothèque pour gérer les fichiers
namespace fs = std::filesystem; // définir un alias pour la bibliothèque
int main()
{
  std::string path = "chemin/vers/le/dossier";
  if (fs::exists(path)) { // utiliser la fonction exists() pour vérifier si le dossier existe
    std::cout << "Le dossier existe!";
  } else {
    std::cout << "Le dossier n'existe pas.";
  }
  return 0;
}
```
Résultat attendu:
```
Le dossier n'existe pas.
```

# Plongée en profondeur:

Il est important de noter que la manière de vérifier si un dossier existe peut varier selon le système d'exploitation utilisé. Par exemple, sur Windows, il est possible que la fonction `fs::exists()` renvoie `true` même si le dossier est vide.

Il existe également d'autres façons de vérifier si un dossier existe, telles que l'utilisation de fonctions spécifiques au système d'exploitation, comme `access()` sur Linux ou `GetFileAttributes()` sur Windows.

Enfin, lors de l'implémentation d'une vérification de dossier, il est important de gérer les erreurs potentielles, comme la non-existence du dossier, afin d'éviter des dysfonctionnements dans le programme.

# Voir aussi:

- Documentation de la [bibliothèque standard C++ sur la gestion des fichiers](https://en.cppreference.com/w/cpp/filesystem)
- Tutoriel sur la [manipulation de fichiers en C++](https://www.geeksforgeeks.org/file-handling-c-classes/)