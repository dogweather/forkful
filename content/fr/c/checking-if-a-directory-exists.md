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

## Quoi et Pourquoi?

Contrôler l'existence d'un répertoire consiste simplement à vérifier si un certain emplacement de fichier spécifié existe déjà ou non. Les programmeurs le font généralement pour éviter les erreurs au moment de l'exécution du programme, comme tenter d'accéder à un répertoire inexistant.

## Comment Faire :

En C, vous pouvez utiliser des fonctions comme `stat()` ou `opendir()` pour vérifier l'existence d'un répertoire. Voici comment vous pouvez le faire :

```C
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int directoryExists(const char* path) {
    struct stat info;

    if(stat(path, &info) != 0)
        return 0;
    else if(info.st_mode & S_IFDIR)
        return 1;
    else
        return 0;
}
```
Lorsque vous exécutez ce bloc de code avec le chemin correct, il retournera 1 si le répertoire existe, et 0 s'il n'existe pas.

## Plongée Profonde :

La méthode mentionnée ci-dessus a des origines historiques dans le système d'exploitation Unix, où la commande `stat` a d'abord été utilisée pour vérifier les informations de fichier.

En termes d'alternatives, vous pouvez également utiliser la fonction `opendir()` qui ouvre un flux de répertoire pour lire. Cette fonction renvoie un pointeur non nul si le répertoire existe.

Dans le détail de l'implémentation, `stat()` renvoie un enregistrement de structure avec information détaillée sur le fichier. `S_IFDIR` est une macro qui est vraie si le fichier est un répertoire.

```C
#include <dirent.h>

int directoryExists(const char* path) {
    DIR* dir = opendir(path);

    if (dir) {
        closedir(dir);
        return 1;
    } else {
        return 0;
    }
}
```
## Voir Aussi :

Pour plus d'information sur les détails et alternatives pour vérifier l'existence d'un répertoire en C :

- [Documentation de la fonction `stat`](https://man7.org/linux/man-pages/man2/stat.2.html) 
- [Documentation de la fonction `opendir`](https://man7.org/linux/man-pages/man3/opendir.3.html)
- [Forum StackOverflow sur le sujet](https://stackoverflow.com/questions/4553012/checking-if-a-file-is-a-directory-in-c)