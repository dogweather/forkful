---
title:    "C: La lecture des arguments en ligne de commande"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un aspect essentiel de la programmation en C. Ils permettent aux utilisateurs de spécifier des paramètres à passer à un programme lors de son exécution. Comprendre comment lire ces arguments est donc crucial pour tout développeur en C.

## Comment Faire

Pour lire les arguments de ligne de commande en C, il faut utiliser la fonction `main()` et ses paramètres `argc` et `argv`. `argc` indique le nombre total d'arguments passés, tandis que `argv` est un tableau contenant ces arguments en tant que chaînes de caractères.

Voici un exemple de code montrant comment lire et afficher les arguments passés :

```C
int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        printf("Argument %d : %s\n", i, argv[i]);
    }
    return 0;
}
```
Supposons que ce programme s'appelle `myprogram` et qu'on l'exécute avec les arguments `hello` et `world`, l'output affichera :

```
Argument 0 : myprogram
Argument 1 : hello
Argument 2 : world
```

Notez que le premier argument `argv[0]` correspond toujours au nom du programme lui-même. 

## Plongée Profonde

Il est également possible de passer des flags (ou indicateurs) en tant qu'arguments de ligne de commande. Un flag est un paramètre qui peut avoir une valeur booléenne pour activer ou désactiver une fonctionnalité spécifique du programme.

Par exemple, en utilisant la bibliothèque standard `<stdbool.h>`, on pourrait créer un flag pour activer le mode debug de notre programme en ajoutant `-d` ou `--debug` en tant qu'argument de ligne de commande.

Voici un exemple de code avec un flag de debug :

```C
#include <stdbool.h>

int main(int argc, char *argv[]) {
    bool debug = false;
    // Vérifie si le flag de debug a été passé
    if (argc > 1 && (strcmp(argv[1], "-d") == 0 || strcmp(argv[1], "--debug") == 0)) {
        debug = true;
    }

    // Utilisation du flag pour afficher un message de débogage
    if (debug) {
        printf("Mode debug activé !\n");
    }

    // Reste du code du programme

    return 0;
}
```

## Voir Aussi

- [Les fonctions de la bibliothèque standard C](https://www.cplusplus.com/reference/cstdlib/)
- [Tutoriel sur l'utilisation des arguments de ligne de commande en C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Documentation officielle de la fonction main() en C](https://en.cppreference.com/w/c/language/main_function)