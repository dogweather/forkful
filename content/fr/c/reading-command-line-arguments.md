---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

La lecture des arguments de la ligne de commande est la méthode qui permet aux programmes C de recevoir des informations pendant l'exécution. Les programmeurs l'utilisent pour rendre les opérations plus dynamiques et interactives.

## Comment Faire:

Voici comment vous pouvez le faire en C. Vous utiliserez les paramètres `argc` et `argv` de la fonction `main()`.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int count;

    printf("Nom du programme '%s'\n", argv[0]);

    if(argc > 1) {
        printf("Il y a %d arguments supplémentaires:\n", argc - 1);

        for(count = 1; count < argc; count++) {
            printf("argv[%d] = %s\n", count, argv[count]);
        }
    } else {
        printf("Aucun argument n'a été fourni.\n");
    }

    return 0;
}
```

Si vous exécutez ce programme avec `./prog arg1 arg2`, l'affichage sera:

```C
Nom du programme './prog'
Il y a 2 arguments supplémentaires:
argv[1] = arg1
argv[2] = arg2
```

## Plongée en profondeur:

Historiquement, le passage d'arguments via la ligne de commande existe depuis les premiers systèmes d'exploitation à interface utilisateur en ligne de commande. En C, des alternatives existent, notamment l'utilisation de fichiers de configuration ou d'entrées utilisateur pendant l'exécution. Toutefois, choisir l'une ou l'autre dépend des exigences spécifiques du programme.

En termes de détails d'implémentation, `argc` et `argv` sont automatiquement passés par le système d'exploitation à votre programme. `argc` est le compteur d'arguments et `argv` est un tableau de pointeurs de chaînes, avec chaque élément pointant vers un argument en ligne de commande.

## Voir aussi:

Pour plus d'informations sur les arguments de ligne de commande, consultez les ressources suivantes:
- GNU C Library documentation: [Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)