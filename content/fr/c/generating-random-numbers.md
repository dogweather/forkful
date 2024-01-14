---
title:                "C: La génération de nombres aléatoires"
simple_title:         "La génération de nombres aléatoires"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation en C, il est toujours bon d'apprendre de nouvelles compétences pour ajouter à votre boîte à outils. L'une de ces compétences utiles est la génération de nombres aléatoires avec le langage de programmation C. La capacité de générer des nombres aléatoires peut être utile dans de nombreux domaines, tels que la simulation de jeux, le chiffrement de données ou la vérification de performances d'algorithmes.

## Comment faire

Pour générer des nombres aléatoires en C, nous utilisons la fonction ```rand()``` du standard C library. Cette fonction renvoie un nombre entier aléatoire entre 0 et ```RAND_MAX```, une constante définie dans la bibliothèque. Cependant, pour obtenir des nombres aléatoires dans une plage différente, nous pouvons utiliser la formule suivante :

```C
// Génère un nombre aléatoire entre min (inclus) et max (exclus)
int rand_num = (rand() % (max - min)) + min;
```

Voyons maintenant un exemple pratique de cette fonction en action :

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    int min = 1, max = 100;
    int rand_num = (rand() % (max - min)) + min;

    printf("Le nombre aléatoire est : %d", rand_num);

    return 0;
}
```

Voici un exemple de sortie possible :

```
Le nombre aléatoire est : 74
```

## Profonde plongée

Alors, comment ```rand()``` génère-t-elle ces nombres aléatoires ? En réalité, elle utilise un algorithme appelé "algorithme congruentiel linéaire". Cet algorithme utilise une formule mathématique pour produire des nombres pseudo-aléatoires, ce qui signifie qu'ils ne sont pas vraiment aléatoires mais apparaissent aléatoires pour un observateur externe. La formule exacte utilisée par ```rand()``` diffère selon les implémentations, mais elle suit généralement cette formule :

```
Xn+1 = (a * Xn + c) % m
```

où ```a, c, m``` sont des constantes et ```Xn``` est le nombre précédent de la séquence. Pour en savoir plus sur le fonctionnement de cet algorithme, voici une explication détaillée (en anglais) : [https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)

## Voir aussi

- [https://www.programiz.com/c-programming/library-function/stdlib.h/rand](https://www.programiz.com/c-programming/library-function/stdlib.h/rand)
- [https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [https://www.oreilly.com/library/view/c-in-a/0596006977/re141.html](https://www.oreilly.com/library/view/c-in-a/0596006977/re141.html)