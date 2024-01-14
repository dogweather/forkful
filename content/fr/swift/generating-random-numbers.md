---
title:                "Swift: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est un aspect fondamental de la programmation Swift. Elle permet aux développeurs de créer des applications interactives et innovantes en utilisant des valeurs aléatoires pour les jeux, les simulations, les tests de performances, etc. Cela ajoute également un élément de surprise et de variété à vos projets.

## Comment faire

Pour générer des nombres aléatoires en Swift, nous pouvons utiliser la fonction `random` de la classe `Int` qui renvoie un nombre aléatoire compris entre 0 et la valeur maximale d'un entier sur votre plateforme. Voici un exemple de code pour générer un nombre aléatoire compris entre 0 et 100 :

```Swift
let randomNumber = Int.random(in: 0...100)
print(randomNumber)
// Output: un nombre aléatoire compris entre 0 et 100
```

Vous pouvez également utiliser la fonction `arc4random_uniform` qui renvoie un nombre aléatoire compris entre 0 et un numéro que vous spécifiez. Par exemple, si vous souhaitez générer un nombre aléatoire compris entre 1 et 10, vous pouvez utiliser cette fonction de la manière suivante :

```Swift
let randomNumber = Int(arc4random_uniform(10)) + 1
print(randomNumber)
// Output: un nombre aléatoire compris entre 1 et 10
```

Vous pouvez également créer une liste de nombres aléatoires en utilisant la fonction `map` avec un intervalle de nombres. Par exemple, si vous voulez créer une liste de 10 nombres aléatoires compris entre 0 et 100, vous pouvez utiliser ce code :

```Swift
let randomNumbers = (1...10).map {_ in Int.random(in: 0...100)}
print(randomNumbers)
// Output: une liste de 10 nombres aléatoires compris entre 0 et 100
```

## Plongée en profondeur

La génération de nombres aléatoires en Swift utilise l'algorithme Mersenne Twister qui est un générateur de nombres pseudo-aléatoires à haute performance et de haute qualité. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais ils sont suffisamment imprédictibles pour être utilisés dans des applications.

Il est important de noter que la valeur initiale utilisée pour générer les nombres aléatoires est appelée "seed" en anglais. Par défaut, cette valeur est générée automatiquement à partir de l'horloge de votre appareil. Cependant, si vous souhaitez générer la même séquence de nombres aléatoires à chaque fois, vous pouvez spécifier une valeur de "seed" en utilisant la fonction `random` avec `RandomNumberGenerator` comme argument.

## Voir aussi

- [La documentation officielle sur la génération de nombres aléatoires en Swift](https://developer.apple.com/documentation/swift/swift_standard_library/numbers/words_and_letters/generating_random_numbers)
- [Le site officiel de Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)
- [Un article sur l'utilisation des nombres aléatoires pour améliorer les tests de performances en Swift](https://www.bignerdranch.com/blog/random-numbers-in-swift/)