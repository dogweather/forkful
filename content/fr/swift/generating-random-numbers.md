---
title:                "Génération de nombres aléatoires"
html_title:           "Swift: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe ou expérimenté, vous avez peut-être entendu parler de la génération de nombres aléatoires en programmation. Mais pourquoi générer des nombres aléatoires en premier lieu ? La réponse est simple : pour créer de la variété et de l'imprévisibilité dans vos programmes ! La génération de nombres aléatoires peut être utile dans des applications telles que les jeux, les simulations ou les tests de performance.

## Comment faire

Pour générer des nombres aléatoires en Swift, il existe une fonction intégrée appelée `random()` qui retourne un nombre aléatoire entre 0 et 1. Vous pouvez ensuite utiliser ce nombre pour créer des valeurs aléatoires dans une plage donnée en multipliant le résultat par la différence entre le nombre maximum et minimum, puis en ajoutant le nombre minimum.

```Swift
let randomNum = Double.random() // génère un nombre aléatoire entre 0 et 1

let randomBetween0And10 = Int.random(in: 0...10) // génère un nombre entier aléatoire entre 0 et 10

let randomDouble = Double.random(in: 50.0...100.0) // génère un nombre décimal aléatoire entre 50 et 100
```

## Plongée en profondeur

Il existe également d'autres méthodes pour générer des nombres aléatoires en Swift, telles que `arc4random()` qui retourne un nombre entier aléatoire dans une plage donnée, ou `arc4random_uniform()` qui retourne un nombre entier aléatoire dans une plage excluant le maximum.

Il est également important de noter que la fonction `random()` ne fournit pas un véritable nombre aléatoire, mais plutôt un nombre pseudo-aléatoire basé sur un algorithme prévisible. Cela signifie qu'il est possible de prédire les valeurs générées si l'algorithme est connu.

## Voir aussi

Si vous souhaitez en savoir plus sur les générateurs de nombres aléatoires en Swift, voici quelques ressources utiles :

- [Documentation officielle Apple sur la génération de nombres aléatoires en Swift](https://developer.apple.com/documentation/swift/random)
- [Un tutoriel du site Hacking with Swift sur la génération de nombres aléatoires](https://www.hackingwithswift.com/example-code/system/how-to-generate-random-numbers-in-swift)
- [Un article de Medium sur les différents types de générateurs de nombres aléatoires en Swift](https://medium.com/@wimdek/true-random-and-pseudo-random-numbers-in-swift-ee422e0bff06)