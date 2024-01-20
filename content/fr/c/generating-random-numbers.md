---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

La génération de nombres aléatoires en C est une technique qui permet de produire des séquences de nombres sans aucun ordre apparent. Les programmeurs l'utilisent pour diverses raisons : test des logiciels, cryptographie, simulations, jeux vidéo et beaucoup plus.

## Comment faire :

Utilisation de la fonction rand() dans la bibliothèque standard C.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  // initialiser le générateur de nombres aléatoires
  srand(time(0));

  // générer un nombre aléatoire et l'affecter à la variable num
  int num = rand();

  // imprimer le résultat
  printf("Le nombre aléatoire est : %d\n", num);

  return 0;
}
```
Output : 
```
Le nombre aléatoire est : (un nombre aléatoire)
```
## Deep Dive:

Historiquement, la génération de nombres aléatoires était une procédure complexe. Aujourd'hui, avec l'évolution des ordinateurs, c'est une tâche simple et parfois transparente. 

En termes d'alternatives, d'autres fonctions comme `random()`, `drand48()`, etc. peuvent être utilisées mais elles sont souvent spécifiques à certaines plates-formes. 

En ce qui concerne les détails de mise en œuvre, `rand()` produit une séquence pseudo-aléatoire. Il est important de noter que sans un bon "seed", comme `time(0)`, la fonction `rand()` produira toujours la même séquence de nombres aléatoires.

## Voir aussi :

- Documentation du C99 Standard Library : https://www.iso.org/standard/29237.html
- Introduction to Randomness and Random Number Generation : https://www.random.org/randomness/
- Génération de nombres aléatoires : https://en.wikipedia.org/wiki/Random_number_generation