---
title:                "C: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est souvent nécessaire en programmation pour stocker des données temporaires ou pour éviter d'écraser des fichiers existants. Cela peut également être utile pour des applications telles que la gestion des caches et des journaux.

## Comment faire

Voici un exemple simple de code en C pour créer un fichier temporaire :

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    // Définition du nom du fichier temporaire
    char *filename = "tempfile.txt";

    // Création du fichier temporaire en mode écriture
    FILE *fp = fopen(filename, "w");

    // Vérification si la création s'est bien déroulée
    if (fp == NULL)
    {
        printf("Impossible de créer le fichier temporaire !");
        exit(1);
    }

    // Écriture de données dans le fichier temporaire
    fprintf(fp, "Ceci est un fichier temporaire créé en utilisant le langage C.");

    // Fermeture du fichier
    fclose(fp);

    // Affichage d'un message de succès
    printf("Le fichier temporaire a été créé avec succès !");

    return 0;
}
```

Ci-dessous, vous trouverez un exemple de sortie pour ce code :

```
Le fichier temporaire a été créé avec succès !
```

## Plongée en profondeur

En utilisant la fonction `fopen()` pour créer un fichier temporaire, celui-ci sera automatiquement supprimé une fois que le programme se terminera. Cela peut être modifié en utilisant la fonction `tmpfile()` qui crée également un fichier temporaire mais qui est géré par le système d'exploitation plutôt que par le programme lui-même.

De plus, il peut être utile de spécifier un répertoire spécifique pour stocker les fichiers temporaires, ce qui peut être fait en utilisant la fonction `mktemp()`.

## Voir aussi

- [Guide complet sur les fichiers en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Documentation officielle sur les fonctions de création de fichiers en C](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [Exemples de code pour la manipulation de fichiers temporaires en C](https://www.geeksforgeeks.org/generating-random-number-range-c/)