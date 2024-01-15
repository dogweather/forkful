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

## Pourquoi

Si vous êtes programmeur, vous savez déjà que les erreurs sont inévitables. L'une des erreurs courantes est d'essayer d'accéder à un fichier ou à un dossier qui n'existe pas. Cela peut entraîner l'échec de votre programme et causer des problèmes à l'utilisateur final. C'est pourquoi il est important de vérifier si un dossier existe avant d'essayer d'y accéder.

## Comment faire

Pour vérifier si un dossier existe en C++, vous pouvez utiliser la fonction `std::filesystem::exists()` de la bibliothèque standard C++17. Voici un exemple simple de code :

```C++
#include <iostream>
#include <filesystem>

int main() {
   if (std::filesystem::exists("chemin/vers/votre/dossier")) {
      std::cout << "Le dossier existe !" << std::endl;
   } else {
      std::cout << "Le dossier n'existe pas." << std::endl;
   }
   return 0;
}
```

Dans cet exemple, nous utilisons la fonction `exists()` pour vérifier si le dossier spécifié existe. Si c'est le cas, nous affichons un message indiquant que le dossier existe, sinon, nous affichons un message indiquant qu'il n'existe pas.

Vous pouvez également utiliser d'autres fonctions telles que `std::filesystem::is_directory()` pour vérifier si un chemin donné est un dossier ou `std::filesystem::is_regular_file()` pour vérifier si c'est un fichier régulier.

## Plongée en profondeur

Maintenant que vous savez comment utiliser la fonction `exists()` pour vérifier si un dossier existe, il est important de connaître ses limitations. Tout d'abord, cette fonction ne fonctionne que pour les chemins absolus ou relatifs, elle ne peut pas vérifier les chemins réseau. Deuxièmement, elle ne peut pas vérifier les droits d'accès au dossier, elle se contente de vérifier si le chemin existe ou non.

De plus, dans certains cas, cette fonction peut retourner un faux positif, c'est-à-dire qu'elle peut indiquer qu'un dossier existe alors qu'en réalité, il n'existe pas. Cela peut se produire si le dossier est temporairement inaccessible en raison de problèmes de verrouillage ou d'autres processus qui utilisent le dossier.

Il est donc important de prendre en compte ces limitations lors de l'utilisation de la fonction `exists()` pour vérifier si un dossier existe.

## Voir aussi

- Documentation sur `exists()` : https://en.cppreference.com/w/cpp/filesystem/exists
- Autres fonctions pour gérer les fichiers et les dossiers en C++ : https://en.cppreference.com/w/cpp/filesystem
- Tutoriel sur la gestion des fichiers et des dossiers en C++ : https://www.geeksforgeeks.org/file-handling-c-classes/