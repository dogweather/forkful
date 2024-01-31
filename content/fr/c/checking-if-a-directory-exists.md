---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Déterminer l'existence d'un dossier, c'est juste checker si un chemin pointe sur un truc qui existe réellement sur le disque. Les devs le font pour éviter des erreurs, genre essayer de lire ou écrire dans un dossier fantôme.

## How to: (Comment faire : )
```C
#include <stdio.h>
#include <sys/stat.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0; // Le chemin n'existe pas ou erreur de lecture
    }
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path = "/chemin/vers/le/dossier";
    if (directory_exists(path)) {
        printf("Yep, le dossier existe.\n");
    } else {
        printf("Nope, le dossier n'est pas là.\n");
    }
    return 0;
}
```
Sortie possible :
```
Yep, le dossier existe.
```
Ou, si le dossier n'existe pas :
```
Nope, le dossier n'est pas là.
```

## Deep Dive (Plongée en profondeur)
Historiquement, la fonction `stat()` est utilisée pour obtenir des infos sur un fichier/dossier. `stat()` remplit une structure avec plein de détails, et tu peux utiliser `S_ISDIR` pour checker si le chemin pointe sur un dossier. Il y a `opendir()` du header `<dirent.h>`, mais c'est moins direct.

Concernant l'implémentation, gaffe aux faux-positifs. `stat()` renvoie 0 si le chemin est un lien symbolique vers un dossier, donc c'est pas parfait. Faut toujours prévoir la gestion d'erreur si `stat()` échoue.

## See Also (Voir Également)
- La page de manuel pour `stat` : [man7.org](https://man7.org/linux/man-pages/man2/stat.2.html)
- La doc GNU C Library sur File Attributes : [gnu.org](https://www.gnu.org/software/libc/manual/html_node/Testing-File-Type.html)
- Tuto sur la gestion des fichiers/dossiers en C : [cprogramming.com](https://www.cprogramming.com/tutorial/cfileio.html)
