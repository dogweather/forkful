---
title:                "Génération de nombres aléatoires"
html_title:           "Haskell: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires est une fonctionnalité importante pour les programmeurs afin d'ajouter de l'incertitude et de la variété dans leurs programmes. Cela peut être utile dans des domaines tels que la sécurité informatique, l'analyse de données et les jeux.

## Comment faire:
Voici un exemple de code Haskell pour générer un nombre aléatoire entre 1 et 10:
```Haskell
import System.Random

randomNumber = randomRIO(1, 10) :: IO Int -- génére un nombre aléatoire entre 1 et 10
```

Voici un autre exemple montrant comment générer un nombre aléatoire à partir d'une liste de valeurs:
```Haskell
import System.Random

list = [5, 10, 15, 20]
randomNumber = randomElem list :: IO Int -- génère un nombre aléatoire à partir de la liste fournie
```

## Plongée en profondeur:
La génération de nombres aléatoires a été un sujet de recherche en informatique depuis de nombreuses années, et il existe de nombreuses méthodes et algorithmes différents pour y parvenir. Certains langages de programmation ont des fonctions intégrées pour générer des nombres aléatoires, mais en Haskell, nous utilisons la bibliothèque `System.Random`.

Une alternative à la bibliothèque `System.Random` est la bibliothèque `random`, qui offre une plus grande flexibilité dans la génération de nombres aléatoires en permettant la création de générateurs personnalisés.

L'implémentation de la génération de nombres aléatoires en Haskell utilise le concept de monades pour encapsuler les valeurs aléatoires dans une structure pouvant être facilement utilisée dans les programmes. La bibliothèque `System.Random` utilise la monade IO tandis que la bibliothèque `random` utilise la monade State.

## Voir aussi:
Pour en savoir plus sur la génération de nombres aléatoires en Haskell, voici quelques liens utiles:
- [Documentation de la bibliothèque `System.Random`](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- [Documentation de la bibliothèque `random`](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html)
- [Un tutoriel sur la génération de nombres aléatoires en Haskell](https://wiki.haskell.org/Random_numbers)
- [Un autre article sur la génération de nombres aléatoires en Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)