---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:55:33.418867-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Lire les arguments de la ligne de commande, ça veut dire récupérer des infos que l'utilisateur passe au programme quand il le démarre. Les programmeurs font ça pour permettre aux utilisateurs de personnaliser l'exécution du programme sans le modifier.

## Comment faire :

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Nombre d'arguments: %d\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Si vous compilez et exécutez ce code avec `./a.out test1 test2`, vous aurez:

```
Nombre d'arguments: 3
Argument 0: ./a.out
Argument 1: test1
Argument 2: test2
```

## Exploration :

Historiquement, lire les arguments de la ligne de commande est aussi vieux que les systèmes d'exploitation Unix-like, où interagir via le terminal était la norme. Les alternatives incluent lire un fichier de configuration ou demander des inputs utilisateur pendant l'exécution. Techniquement, `argc` représente le nombre d'arguments, et `argv` est un tableau de chaînes de caractères (`strings`) qui contient les arguments proprement dit. Le premier argument, `argv[0]`, est toujours le nom du programme.

## Voir aussi :

Pour plus de détails sur la manipulation des arguments de la ligne de commande en C, consultez ces sources :

- Le chapitre correspondant dans le livre "C Programming Language" de Brian W. Kernighan et Dennis M. Ritchie : https://www.ime.usp.br/~pf/Kernighan-Ritchie/C-Programming-Ebook.pdf
- Documentation GNU sur les conventions pour les interfaces de ligne de commande : https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html
- Tutorial interactif sur la gestion des arguments de ligne de commande avec `getopt` : https://www.gnu.org/software/libc/manual/html_node/Getopt.html
