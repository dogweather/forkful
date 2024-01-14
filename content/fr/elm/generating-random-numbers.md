---
title:    "Elm: Générer des nombres aléatoires"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une compétence importante en programmation pour une variété de raisons, notamment la simulation, les tests de logiciel et les jeux. Apprenons comment le faire en utilisant le langage de programmation fonctionnelle Elm.

## Comment faire

Nous pouvons utiliser la fonction `Random.step` pour générer un entier aléatoire entre deux valeurs données. Dans l'exemple suivant, nous allons générer un nombre entre 1 et 10 :

```Elm
import Random

Random.step (Random.int 1 10) Random.initialSeed
```

La sortie de cet exemple pourrait être un entier aléatoire tel que `4` ou `8`.

Nous pouvons également utiliser la fonction `Random.range` pour générer un flottant aléatoire entre deux valeurs données. Par exemple, pour générer un nombre entre 0 et 1 :

```Elm
import Random

Random.range 0 1 Random.initialSeed
```

La sortie de cet exemple pourrait être un nombre décimal aléatoire tel que `0.745312` ou `0.132148`.

Il est également possible de générer des nombres aléatoires dans une liste en utilisant la fonction `Random.list`. Par exemple, si nous voulons générer une liste de 5 nombres entiers aléatoires compris entre 1 et 100 :

```Elm
import Random

Random.list 5 (Random.int 1 100) Random.initialSeed
```

La sortie de cet exemple pourrait être une liste telle que `[52, 8, 34, 91, 16]`.

## Plongeon en profondeur

La fonction `Random.step` utilise un `Random.Generator`, qui est une façon de générer une valeur aléatoire à partir d'une graine (seed). Chaque fois que nous utilisons `Random.step` avec la même graine, nous obtenons la même valeur aléatoire. Nous pouvons également utiliser la fonction `Random.initialSeed` pour générer une graine initiale et utiliser cette graine pour générer plusieurs valeurs aléatoires.

En plus des fonctions mentionnées ci-dessus, Elm possède de nombreuses autres options pour générer des nombres aléatoires, y compris des chaînes de caractères aléatoires et des valeurs personnalisées.

## Voir aussi

- [Documentation Elm sur la génération de nombres aléatoires](https://guide.elm-lang.org/effects/random.html)
- [Article sur les nombres aléatoires en Elm](https://dev.to/emmanuelantony2000/generating-random-numbers-in-elm-11lf)