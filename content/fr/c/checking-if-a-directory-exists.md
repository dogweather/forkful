---
title:    "C: Vérifier si un répertoire existe"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans un programme C, il peut être utile de vérifier si un répertoire existe avant de tenter de l'utiliser. Cela permet de s'assurer que le programme ne rencontre pas d'erreur lorsqu'il essaie d'accéder à un répertoire qui n'existe pas.

## Comment Faire

La vérification de l'existence d'un répertoire peut être réalisée à l'aide de la fonction `opendir()` de la bibliothèque standard C. Cette fonction permet d'ouvrir un répertoire et renvoie un pointeur vers celui-ci. Si le répertoire n'existe pas, le pointeur renvoyé sera nul. Voici un exemple de code montrant comment vérifier l'existence d'un répertoire :

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Répertoire à vérifier
    char *path = "/chemin/vers/mon/répertoire";
    
    // Tentative d'ouverture du répertoire
    DIR *dir = opendir(path);
    
    // Si opendir renvoie NULL, le répertoire n'existe pas
    if (dir == NULL) {
        printf("Le répertoire n'existe pas !\n");
    } else {
        printf("Le répertoire existe !\n");
        closedir(dir);
    }
    
    return 0;
}
```
Voici un exemple de sortie pour un répertoire qui existe :

```
Le répertoire existe !
```

Et pour un répertoire qui n'existe pas :

```
Le répertoire n'existe pas !
```

## Plongée Profonde

Il est important de noter que cette méthode ne permet que de vérifier l'existence d'un répertoire. Elle ne garantit pas que l'utilisateur ait les permissions nécessaires pour accéder au répertoire, ni même que le répertoire soit un répertoire valide dans le système de fichiers. De plus, la fonction `opendir()` peut également échouer pour d'autres raisons, comme un manque de mémoire disponible. Il est donc recommandé de toujours gérer les erreurs lorsque l'on utilise cette fonction.

Une autre méthode couramment utilisée pour vérifier l'existence d'un répertoire consiste à utiliser la fonction `stat()` de la bibliothèque standard C. Celle-ci permet d'obtenir des informations sur un fichier ou répertoire, telles que sa taille ou sa date de modification. Elle renvoie un code d'erreur spécifique (`-1`) si le fichier ou répertoire n'existe pas. Voici un exemple de code utilisant cette méthode :

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    // Répertoire à vérifier
    char *path = "/chemin/vers/mon/répertoire";
    
    // Structure pour stocker les infos de stat
    struct stat st;
    
    // Tentative d'obtention des infos de stat
    int result = stat(path, &st);
    
    // Si stat renvoie -1, le répertoire n'existe pas
    if (result == -1) {
        printf("Le répertoire n'existe pas !\n");
    } else {
        printf("Le répertoire existe !\n");
    }
    
    return 0;
}
```

## Voir aussi

- [Documentation de la fonction opendir()](https://man7.org/linux/man-pages/man3/opendir.3.html)
- [Documentation de la fonction stat()](https://man7.org/linux/man-pages/man2/stat.2.html)