---
title:                "Création d'un fichier temporaire"
html_title:           "C++: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Créer un fichier temporaire en programmation consiste à créer un fichier qui est supprimé automatiquement après son utilisation. Les programmeurs le font souvent pour stocker temporairement des données ou des résultats avant de les traiter ou de les transférer vers un autre fichier.

## Comment faire :
Voici un exemple de code pour créer un fichier temporaire en C++ :
```C++
#include <cstdio>

int main() 
{
  FILE * tempFile;
  tempFile = tmpfile(); // Crée un fichier temporaire
  if (tempFile)
  {
    fputs("Données à stocker temporairement", tempFile);
    // Autres opérations sur le fichier temporaire
    fclose(tempFile); // Ferme et supprime le fichier temporaire
  }
  return 0;
}
```
Voici un exemple de sortie de ce code :
```
$ ls
a.out    // Fichier compilé du code ci-dessus
$ ./a.out
$ ls
// Aucun fichier temporaire visible, car il a été supprimé automatiquement après utilisation
a.out
```

## Plongée en profondeur :
Avant la création de fichiers temporaires, les programmeurs utilisaient souvent des techniques telles que l'utilisation de dossiers spécifiques pour stocker temporairement des fichiers ou utiliser des numéros ou noms de fichiers aléatoires. Cependant, ces méthodes n'étaient pas aussi efficaces que la création de fichiers temporaires, car elles pouvaient être sujettes à des erreurs ou échouer dans certains cas. La création de fichiers temporaires est désormais une méthode courante et sûre pour stocker temporairement des données en programmation.

Il existe également des alternatives à la création de fichiers temporaires en C++. Par exemple, il est possible d'utiliser des variables temporaires pour stocker les données, mais cela peut être limitant en termes de mémoire disponible. L'utilisation de la bibliothèque standard <fstream> avec des objets de type stringstream est également une option, mais cela nécessite plus de code et peut être plus complexe pour les débutants. La création de fichiers temporaires reste donc la méthode privilégiée pour stocker temporairement des données en C++.

La création de fichiers temporaires en C++ est implémentée grâce à des fonctions de la bibliothèque standard, telles que tempnam(), mktemp() et tmpfile(). Ces fonctions génèrent un nom de fichier aléatoire dans un emplacement spécifique et créent un fichier à cet emplacement. Une fois le fichier utilisé et fermé, il est automatiquement supprimé par le système d'exploitation.

## Voir aussi :
Pour plus d'informations sur la création de fichiers temporaires en C++, vous pouvez consulter les liens suivants :
- Documentation de la bibliothèque standard C++ : https://en.cppreference.com/w/cpp/io/c/tmpfile
- Tutoriel sur les fichiers temporaires en C++ : https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm