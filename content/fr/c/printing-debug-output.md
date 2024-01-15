---
title:                "Imprimer la sortie de débogage"
html_title:           "C: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe, vous avez sûrement déjà entendu parler de l'importance des messages de débogage. Mais pourquoi est-il si important de les incorporer dans votre code? Eh bien, cela peut sembler évident, mais les messages de débogage peuvent grandement faciliter la détection et la résolution des erreurs dans votre programme. Ils vous permettent de suivre le flux d'exécution et de vérifier les valeurs des variables à différents points du code, ce qui peut vous aider à identifier la source d'un bug plus rapidement et efficacement.

## Comment faire

La bonne nouvelle est qu'il est très simple d'ajouter des messages de débogage dans votre code en utilisant le langage C. Tout ce que vous avez à faire est d'utiliser la fonction `printf()` avec le spécificateur `%d` pour afficher la valeur d'une variable entière ou `%f` pour une variable de type flottant. Voici un exemple de code pour illustrer cela:

```
#include <stdio.h>

int main() {

   int age = 25;

   printf("Mon âge est de %d ans.\n", age);

   float poids = 70.5;

   printf("Mon poids est de %f kg.\n", poids);

   return 0;
}
```

La ligne de code `printf("Mon âge est de %d ans.\n", age);` affichera "Mon âge est de 25 ans." dans la console, tandis que `printf("Mon poids est de %f kg.\n", poids);` affichera "Mon poids est de 70.500000 kg.".

## Plongeon en profondeur

Il existe différentes façons d'ajouter des messages de débogage dans votre code en utilisant la fonction `printf()` et ses spécificateurs. Par exemple, vous pouvez utiliser `%s` pour afficher une chaîne de caractères ou `%c` pour un caractère unique. De plus, vous pouvez également afficher plusieurs variables en une seule fois en les séparant par des virgules, comme dans cet exemple:

```
int nombre = 10;
char lettre = 'A';

printf("Voici un nombre: %d et une lettre: %c.\n", nombre, lettre);
```

Cette ligne de code affichera "Voici un nombre: 10 et une lettre: A." dans la console.

N'oubliez pas que les messages de débogage doivent être retirés de votre code avant de le déployer pour une utilisation réelle, sinon ils pourraient ralentir considérablement votre programme.

## Voir aussi

Maintenant que vous en savez davantage sur l'utilisation des messages de débogage en langage C, voici quelques liens supplémentaires qui pourraient vous intéresser:

- [Documentation officielle de la fonction printf() en langage C](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)
- [Un tutoriel interactif sur les messages de débogage en C](https://www.learn-c.org/en/Debugging)
- [Un article sur l'optimisation des messages de débogage en C](https://www.infoworld.com/article/3303844/debugging-print-tactics-and-how-to-use-them-to-solve-programming-problems.html)