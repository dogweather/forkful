---
title:                "Lecture des arguments de ligne de commande"
html_title:           "C: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Qu'est-ce que la lecture des arguments de la ligne de commande et pourquoi les programmeurs le font-ils?

Lorsque vous exécutez un programme à partir de la ligne de commande, vous pouvez lui fournir des arguments en plus de son nom. La lecture des arguments de la ligne de commande est essentielle pour permettre à votre programme de prendre en compte ces arguments et de les utiliser dans son fonctionnement. Les programmeurs le font pour rendre leurs programmes plus flexibles et leur permettre de prendre en charge différentes configurations.

Comment faire :

```C
#include <stdio.h>

int main(int argc, char *argv[]) {

    printf("Voici les arguments de la ligne de commande : \n");

    for(int i = 0; i < argc; i++) {
        printf("%s\n", argv[i]);
    }

    return 0;
}
```

Exemple de sortie :

```
$ ./arguments azerty 123 true
Voici les arguments de la ligne de commande :
./arguments
azerty
123
true
```

Zoom sur :

La lecture des arguments de la ligne de commande n'est pas une pratique nouvelle en programmation. Elle remonte à l'époque des premiers systèmes d'exploitation. Les alternatives à la lecture des arguments de la ligne de commande incluent l'utilisation de variables d'environnement ou encore de fichiers de configuration. En implémentant la lecture des arguments de la ligne de commande dans votre programme, vous pouvez permettre à l'utilisateur de le personnaliser sans avoir à modifier en profondeur les variables d'environnement ou à utiliser des fichiers de configuration. Cela peut également rendre votre programme plus accessible et plus facile à utiliser pour les débutants.

Voir aussi :

- [Argument (computing)](https://en.wikipedia.org/wiki/Command-line_interface#Arguments) sur Wikipedia
- [Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/) sur GeeksforGeeks