---
title:                "C: Vérification de l'existence d'un répertoire"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vérifier si un répertoire existe est une tâche courante en programmation. Cela peut être utile pour s'assurer qu'un chemin de fichier est valide avant de créer ou d'y accéder. Cela peut également être utilisé pour vérifier si un répertoire spécifique a déjà été créé.

## Comment faire

Pour vérifier si un répertoire existe en C, nous utilisons la fonction `opendir()` qui prend en paramètre le chemin du répertoire à vérifier. Voici un exemple de code :

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    char* path = "/chemin/vers/mon/repertoire";
    
    // Vérification du répertoire
    DIR* dir = opendir(path);
    
    // Si le répertoire existe
    if (dir) {
       printf("Le répertoire existe !");
       closedir(dir);
    }
    // Si le répertoire n'existe pas
    else {
        printf("Le répertoire n'existe pas !");
    }
    
    return 0;
}
```

Lors de l'exécution de ce code, si le répertoire spécifié existe, un message indiquant sa présence sera affiché. Sinon, un autre message indiquant qu'il n'existe pas sera affiché.

## Approfondissement

La fonction `opendir()` utilise un pointeur de type `DIR` pour représenter le répertoire ouvert. Ce pointeur peut ensuite être utilisé pour effectuer d'autres opérations sur ce répertoire si besoin.

Il est également important de noter que la fonction `opendir()` renvoie `NULL` en cas d'erreur. Il est donc important de toujours vérifier si le répertoire a été ouvert avec succès avant de continuer à utiliser le pointeur de type `DIR`.

## Voir aussi

- [Fonction opendir() en C](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Manipulation de fichiers et répertoires en C](https://www.commentcamarche.net/contents/754-manipulation-de-fichiers-et-repertoires-en-c)
- [Gestion des erreurs en C](https://fr.wikibooks.org/wiki/Programmation_C/Gestion_des_erreurs)