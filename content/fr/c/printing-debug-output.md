---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Impression de débogage dans C : Un guide simple

## Quoi & Pourquoi ?
L’impression de débogage c'est une méthode qui nous permet d'afficher des valeurs de variables ou des messages à l'écran durant l'exécution d'un programme. Cette technique sert aux programmeurs pour comprendre le comportement d'un programme, pour identifier et résoudre les bugs.

## Comment faire :
Voici un exemple simple de comment réaliser une impression de débogage en utilisant la fonction `printf()` de la bibliothèque standard C :

```C
#include <stdio.h>

int main() {
    int x = 5;
    printf("Débogage : x = %d\n", x);
    return 0;
}
```

Dans ce cas, l'impression de débogage vous montrera `Débogage : x = 5`. Cela peut vous aider à suivre la valeur de la variable `x` pendant l'exécution du programme.

## Immersion profonde 
L'impression de débogage est une technique qui existait bien avant le débogueur intégré aux IDE comme Eclipse ou IntelliJ. C'est une technique universelle et indépendante de la langue ou de l'environnement de développement.

Il existe beaucoup d'alternatives à l'impression de débogage. Il y a d'abord les débogueurs intégrés mentionnés plus haut, mais aussi des outils comme GDB. De plus, certaines langues proposent des fonctions intégrées pour la journalisation et le débogage.

Dans le cas de C, la fonction `printf()` est généralement utilisée pour l'impression de débogage. Mais attention, il faut bien penser à enlever ou commenter ces instructions une fois le débogage terminé pour ne pas impacter la performance du programme.

## Voir aussi
- [Documentation de la fonction printf()](https://www.cplusplus.com/reference/cstdio/printf/)
- [Guide d'utilisation de GDB](https://sourceware.org/gdb/current/onlinedocs/gdb/)
- [Forum StackOverflow pour C](https://stackoverflow.com/questions/tagged/c)