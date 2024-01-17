---
title:                "Afficher la sortie de débogage"
html_title:           "C: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?

L'impression de sortie de débogage est une technique couramment utilisée par les programmeurs pour suivre le déroulement d'un programme et vérifier son comportement. Cela consiste à afficher des informations relatives à l'exécution du code, telles que les valeurs des variables ou les étapes de calcul. Cela peut être particulièrement utile pour comprendre et résoudre les erreurs dans le code.

## Comment faire :

Voici un exemple simple de code en C qui utilise l'impression de sortie de débogage :

```
#include <stdio.h>

int main() {
    int x = 5;
    int y = 3;
    int z = x + y;

    printf("x = %d, y = %d, z = %d \n", x, y, z);

    return 0;
}
```

En exécutant ce code, vous verrez en sortie les valeurs des variables `x`, `y`, et `z` imprimées à l'écran. Cela peut vous aider à comprendre la logique derrière le calcul de `z` et à détecter d'éventuels problèmes.

## Plongée en profondeur :

L'impression de sortie de débogage est une pratique courante depuis les débuts de la programmation informatique. Cela permet aux programmeurs de mieux comprendre leur code et de le corriger plus rapidement. Toutefois, il existe d'autres méthodes pour déboguer un programme, telles que l'utilisation d'un débogueur (debugger) ou d'un profilage (profiler) pour mesurer les performances du code. Néanmoins, l'impression de sortie de débogage reste une méthode simple et efficace pour comprendre le comportement d'un programme et identifier les erreurs.

En termes d'implémentation, chaque langage de programmation a sa propre syntaxe pour l'impression de sortie de débogage. En C, on utilise la fonction `printf()` de la bibliothèque standard pour afficher des informations à l'écran. Il est également possible d'utiliser des macros spécifiques, telles que `assert()` ou `debug()`, pour envoyer des messages de débogage à la console.

## Voir aussi :

- [Débogueur (Wikipedia)](https://fr.wikipedia.org/wiki/D%C3%A9bogueur)
- [Profilage (Wikipedia)](https://fr.wikipedia.org/wiki/Profilage_%28informatique%29)