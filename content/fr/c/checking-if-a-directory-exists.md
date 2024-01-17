---
title:                "Vérifier si un répertoire existe"
html_title:           "C: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce-que et Pourquoi?

Vérifier si un répertoire existe est un moyen pour les programmeurs de s'assurer qu'un chemin d'accès spécifié mène à un répertoire valide. Cela permet de s'assurer que le programme peut accéder aux fichiers et dossiers nécessaires pour son exécution sans rencontrer d'erreurs.

## Comment faire:

Il est possible de vérifier si un répertoire existe en utilisant la fonction stat() de la bibliothèque standard en C. Elle prend en entrée le nom du répertoire et renvoie une structure stat contenant des informations sur le fichier ou le répertoire spécifié. Si le répertoire existe, la structure sera remplie et la fonction renverra 0 en tant que valeur de retour. Sinon, elle renverra -1.

```C
#include <stdio.h>
#include <sys/stat.h>

int main()
{
    char* directory = "path/to/directory";
    struct stat info;
    if(stat(directory, &info) == 0)
    {
        printf("%s existe. \n", directory);
    }
    else
    {
        printf("%s n'existe pas. \n", directory);
    }
    return 0;
}
```

## Plongée en profondeur:

Avant la version actuelle du langage C, il n'y avait pas de fonction spécifique pour vérifier si un répertoire existait. Les programmeurs devaient utiliser d'autres méthodes telles que opendir() et readdir() pour parcourir les fichiers et répertoires et vérifier s'ils correspondaient au chemin d'accès spécifié. Cependant, ces méthodes n'étaient pas aussi efficaces et fiables que la fonction stat() actuelle.

Il existe également d'autres méthodes pour vérifier si un répertoire existe, telles que l'utilisation de POSIX (Portable Operating System Interface) et la fonction access(), qui renvoie également 0 si le répertoire existe. Cependant, la fonction stat() reste la méthode recommandée pour sa fiabilité et sa compatibilité avec les systèmes d'exploitation.

## Voir aussi:

- La documentation officielle de la fonction stat() en C: https://www.gnu.org/software/libc/manual/html_node/File-Status-Test-Macros.html#File-Status-Test-Macros
- Un guide détaillé sur la manipulation de fichiers et répertoires en C: https://www.geeksforgeeks.org/c-programming-language/.