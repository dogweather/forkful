---
title:                "Génération de nombres aléatoires"
html_title:           "Gleam: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est une fonctionnalité essentielle pour de nombreux programmes informatiques, en particulier lorsqu'il s'agit de jeux, de simulations ou de tests. Elle permet d'introduire une certaine variabilité dans le comportement du programme, ce qui le rend plus intéressant et plus réaliste pour les utilisateurs.

## Comment faire

Pour générer des nombres aléatoires en utilisant le langage de programmation Gleam, vous pouvez utiliser la bibliothèque standard `random` en l'important avec `import random`.

Ensuite, vous pouvez utiliser la fonction `random.int` pour générer un nombre aléatoire entre deux valeurs spécifiées. Par exemple, pour générer un nombre aléatoire entre 1 et 10, vous pouvez utiliser le code suivant :

```
Gleam import random

let random_number = random.int(1, 10)
```

Vous pouvez également générer des nombres aléatoires à virgule flottante en utilisant la fonction `random.float`, qui prend en compte une limite supérieure (exclue) en plus de la limite inférieure (incluse). Par exemple :

```
Gleam import random

let random_float = random.float(0.0, 1.0)
```

Le code ci-dessus générera un nombre aléatoire compris entre 0.0 inclus et 1.0 exclu.

## Plongée profonde

La génération de nombres aléatoires utilise des algorithmes et des techniques mathématiques complexes pour garantir que les nombres générés sont vraiment aléatoires. Cependant, il est important de noter que les nombres générés par les ordinateurs ne sont jamais complètement aléatoires, mais plutôt pseudo-aléatoires.

Pour en savoir plus sur les différents types de génération de nombres aléatoires et sur la façon dont Gleam les implémente, vous pouvez consulter la documentation de la bibliothèque standard sur `random` ainsi que d'autres ressources en ligne telles que des tutoriels et des articles.

## Voir aussi

- Documentation sur la bibliothèque standard `random` : [lien vers la documentation]
- Tutoriel sur la génération de nombres aléatoires en Gleam : [lien vers le tutoriel]
- Article sur les différents types de génération de nombres aléatoires : [lien vers l'article]