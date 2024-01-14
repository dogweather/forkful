---
title:    "Swift: Génération de nombres aléatoires"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler être une tâche sans intérêt, mais c'est en fait un élément crucial dans de nombreux programmes. La génération de nombres aléatoires est utilisée dans les jeux, les simulations, les tests de performance, et bien plus encore.

## Comment faire

La programmation en Swift offre différentes manières de générer des nombres aléatoires. Voici un exemple simple utilisant la fonction `random()` :

```Swift
let nombreAleatoire = Int.random(in: 1...10)
print(nombreAleatoire) // affiche un nombre aléatoire entre 1 et 10
```

On peut également utiliser la fonction `arc4random_uniform()` pour générer des nombres aléatoires dans une plage donnée :

```Swift
let nombreAleatoire = Int(arc4random_uniform(100))
print(nombreAleatoire) // affiche un nombre aléatoire entre 0 et 99
```

Pour générer un nombre aléatoire de type `Double`, on peut utiliser la fonction `Double.random(in: min...max)`. Par exemple :

```Swift
let nombreAleatoire = Double.random(in: 1.5...5.0)
print(nombreAleatoire) // affiche un nombre aléatoire entre 1.5 et 5.0
```

## Plongée en profondeur

La génération de nombres aléatoires peut sembler simple, mais c'est en fait un processus complexe qui utilise des algorithmes sophistiqués pour garantir une répartition égale et aléatoire des nombres. Il est également important de prendre en compte la "graine" utilisée pour générer les nombres aléatoires, qui peut affecter leur prédictibilité.

Il existe également des bibliothèques et des packages en Swift qui offrent des fonctions plus avancées pour la génération de nombres aléatoires, telles que la génération de nombres aléatoires basés sur des distributions spécifiques (gaussienne, binomiale, etc.).

## Voir aussi

- [Documentation officielle de Swift sur la génération de nombres aléatoires](https://developer.apple.com/documentation/swift/numeric/random_numbers)
- [RandSwift, un package pour la génération de nombres aléatoires en Swift](https://github.com/ekscrypto/RandSwift)
- [Un tutoriel sur la génération de nombres aléatoires en Swift](https://www.swiftbysundell.com/articles/random-numbers-in-swift/)