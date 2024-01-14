---
title:                "C++: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire?

Créer un fichier temporaire peut être utile pour stocker des données temporaires pendant l'exécution d'un programme, ou pour effectuer une opération de sauvegarde. Cela peut également être utile pour tester certaines fonctionnalités sans perturber les fichiers existants.

## Comment créer un fichier temporaire en C++

Il existe plusieurs façons de créer un fichier temporaire en C++. La méthode la plus courante consiste à utiliser la fonction `tmpfile()` qui crée un fichier vide dans le répertoire temporaire du système. Voici un exemple de code:

```C++
#include <cstdio>

int main() {
  // Création d'un fichier temporaire
  FILE* file = tmpfile();
  
  // Vérification si le fichier a été créé avec succès
  if (file != nullptr) {
    // Écriture de données dans le fichier
    fprintf(file, "Hello world!");
    
    // Fermeture du fichier
    fclose(file);
  }
  
  return 0;
}
```

En utilisant la fonction `fprintf()`, nous pouvons écrire des données dans le fichier temporaire. Il est important de noter que le fichier sera automatiquement supprimé à la fermeture du programme.

## Plongée dans la création de fichiers temporaires

Il est possible de créer un fichier temporaire dans un répertoire spécifique en utilisant la fonction `tmpnam()`. Cette fonction génère un nom de fichier unique basé sur le nom du répertoire spécifié. Voici un exemple de code pour créer un fichier temporaire dans le répertoire "temp":

```C++
#include <cstdio>
#include <cstdlib>

int main() {
  // Création d'un chemin vers le répertoire temporaire
  char tempDir[] = "temp";
  
  // Création d'un nom de fichier temporaire
  char* tempFilename = tmpnam(tempDir);
  
  // Création du fichier temporaire
  FILE* file = fopen(tempFilename, "w");
  
  // Vérification si le fichier a été créé avec succès
  if (file != nullptr) {
    // Écriture de données dans le fichier
    fputs("Bonjour le monde!", file);
    
    // Fermeture du fichier
    fclose(file);
  }
  
  return 0;
}
```

Il est également possible de supprimer manuellement un fichier temporaire en utilisant la fonction `remove()`. Cela peut être utile si vous n'avez pas besoin du fichier dans certaines parties de votre programme.

```
#include <cstdio>
#include <cstdlib>

int main() {
  // Création d'un fichier temporaire
  FILE* file = tmpfile();
  
  // Suppression du fichier
  remove("fichier_temporaire");
  
  return 0;
}
```

## Voir aussi
- [Documentation sur la fonction tmpfile() en C++](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Guide sur la création et la gestion de fichiers en C++](https://www.learncpp.com/cpp-tutorial/61-working-with-files/)
- [Fonctions pour la gestion de fichiers en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)