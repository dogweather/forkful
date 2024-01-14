---
title:    "C: Génération de nombres aléatoires"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une compétence importante pour tout programmeur en C. Cela permet de créer des simulations, des jeux, des tests aléatoires et bien plus encore. Apprenons ensemble comment générer des nombres aléatoires en C !

## Comment faire

Pour générer des nombres aléatoires en C, il y a plusieurs étapes à suivre :

1. Inclure le fichier d'en-tête `stdlib.h` qui contient la fonction `rand()` pour générer des nombres aléatoires.
2. Utiliser la fonction `srand()` pour initialiser le générateur de nombres aléatoires.
3. Utiliser la fonction `rand()` pour générer un nombre aléatoire compris entre 0 et `RAND_MAX`.

En voici un exemple de code :

```
#include <stdio.h>
#include <stdlib.h>

int main() {
  // initialiser le générateur de nombres aléatoires
  srand(time(0));
  // générer un nombre aléatoire compris entre 0 et 10
  int nombre_aleatoire = rand() % 11;
  // afficher le nombre généré
  printf("Le nombre aléatoire est : %d\n", nombre_aleatoire);
  return 0;
}

```

Et voici un exemple de sortie :

```
Le nombre aléatoire est : 7
```

## Plongeons plus profondément

Cela peut sembler simple, mais la génération de nombres aléatoires est en fait basée sur l'utilisation d'un "seed" (ou graine) qui sert à initialiser le générateur de nombres aléatoires. Par défaut, ce seed est généralement la date et l'heure actuelles, mais il est également possible de définir un seed manuellement en utilisant la fonction `srand()`.

Il est également important de noter que les nombres générés par la fonction `rand()` sont en réalité pseudo-aléatoires, c'est-à-dire qu'ils sont produits de manière déterministe à partir du seed initial. Cela signifie que si vous utilisez le même seed, vous obtiendrez toujours la même série de nombres aléatoires. Cela peut être utile pour déboguer votre code, mais n'oubliez pas de réinitialiser le seed avant de passer à la production.

De plus, la fonction `rand()` ne génère que des nombres entiers. Si vous avez besoin de nombres réels, vous pouvez utiliser la fonction `rand()` en conjonction avec la fonction `srand()` et des opérations arithmétiques pour obtenir des nombres décimaux.

## Voir aussi

- Guide complet de la génération de nombres aléatoires en C : https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/
- Exemple de projet utilisant la génération de nombres aléatoires en C : https://github.com/vivek9patel/Random-Number-Generator
- Tutoriel vidéo pour générer des nombres aléatoires en C : https://youtu.be/k2ztgP2-U_s