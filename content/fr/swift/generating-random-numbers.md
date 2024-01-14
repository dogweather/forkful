---
title:                "Swift: Production de nombres aléatoires"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est une compétence essentielle dans la programmation Swift. Elle est principalement utilisée pour créer des jeux, déterminer des gagnants de tirage au sort ou pour tester des fonctionnalités aléatoires dans une application. Apprenez à générer des nombres aléatoires dans cette article en utilisant Swift !

## Comment faire

Voici comment générer un nombre aléatoire en Swift :

```Swift
// Générer un nombre aléatoire entre 1 et 100
let randomNumber = Int.random(in: 1...100)
print(randomNumber)

// Générer un nombre aléatoire entre 0 et 1
let randomDecimal = Double.random(in: 0...1)
print(randomDecimal)
```

Voici un exemple de sortie de code :

```Swift
37
0.736593
```

Vous pouvez également utiliser la fonction `arc4random_uniform()` pour générer des nombres entiers aléatoires :

```Swift
// Générer un nombre aléatoire entre 1 et 10
let randomNumber = Int(arc4random_uniform(10)) + 1
print(randomNumber)
```

## Plongée en profondeur

Maintenant que vous savez comment générer des nombres aléatoires en Swift, voici quelques conseils pour les utiliser efficacement :

- Utilisez la boucle `for` pour générer plusieurs nombres aléatoires en une seule fois.
- Vous pouvez également utiliser des conditions `if` pour contrôler la génération de nombres aléatoires selon certaines conditions.
- N'oubliez pas de convertir les nombres aléatoires en types de données appropriés si nécessaire.

## Voir aussi

- [Documentation Apple sur la génération de nombres aléatoires en Swift](https://developer.apple.com/documentation/swift/swift_standard_library/numbers_and_basic_values/generating_random_numbers)
- [Tutoriel sur la génération de nombres aléatoires en Swift](https://www.hackingwithswift.com/example-code/math/how-to-generate-random-numbers-in-swift)
- [Exemples pratiques de génération de nombres aléatoires en Swift](https://www.avanderlee.com/swift/random-numbers/)

Merci d'avoir lu cet article sur la génération de nombres aléatoires en Swift ! N'hésitez pas à explorer davantage ou à utiliser cette compétence dans vos futurs projets. À bientôt !