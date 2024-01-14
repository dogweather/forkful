---
title:    "C: Vérifier si un répertoire existe"
keywords: ["C"]
---

{{< edit_this_page >}}

# Pourquoi
La vérification de l'existence d'un répertoire peut être un élément important dans un programme pour s'assurer que le chemin d'accès fourni est valide avant de poursuivre les opérations.

## Comment faire
La vérification de l'existence d'un répertoire peut être réalisée en utilisant la fonction `opendir()` de la bibliothèque standard C. Cette fonction prend un chemin d'accès en paramètre et renvoie un pointeur de type `DIR` si le répertoire existe ou NULL s'il n'existe pas.

Exemple de code:

```C
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main()
{
    // chemin d'accès à vérifier
    char *chemin = "/chemin/dossier";

    // tentative d'ouverture du répertoire
    DIR *rep = opendir(chemin);

    // si l'ouverture a échoué, afficher un message
    if (rep == NULL)
    {
        printf("Le répertoire n'existe pas\n");
    }
    // sinon, le répertoire existe, afficher un message
    else
    {
        printf("Le répertoire existe\n");
    }

    return 0;
}
```

Exemple de sortie:

```
Le répertoire existe
```

## Exploration en profondeur
Il est important de comprendre que la vérification de l'existence d'un répertoire ne garantit pas que vous avez accès à ce répertoire. La fonction `opendir()` vérifie simplement si le répertoire existe, mais elle ne vérifie pas les permissions ou les privilèges nécessaires pour y accéder. Il est donc possible que même si le répertoire existe, votre programme soit toujours incapable de l'ouvrir pour effectuer des opérations.

Il est également important de noter que même si vous pouvez vérifier si un répertoire existe ou non, vous ne pouvez pas en créer un avec la fonction `opendir()`. Cette fonction ne prend en charge que l'ouverture de répertoires existants.

# Voir aussi
- [Documentation de la fonction opendir()](https://linux.die.net/man/3/opendir)
- [Tutoriel sur les répertoires en C](https://www.youtube.com/watch?v=a8vvuYSqNP0)
- [Exemples de manipulation de répertoires en C](https://www.programmingsimplified.com/c-program-create-delete-directory)