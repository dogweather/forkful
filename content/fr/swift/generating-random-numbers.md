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

## Qu'est-ce que c'est et pourquoi le fait-on?
Générer des nombres aléatoires est une pratique courante dans la programmation où l'on souhaite créer des valeurs uniques et imprévisibles. Cela peut être utile pour des jeux, des simulations ou encore des tests.

## Comment faire:
Voici un exemple de code en Swift pour générer un nombre aléatoire entre 1 et 10 et l'afficher à l'écran:

```
let randomNum = Int.random(in: 1...10)
print(randomNum) //output: un nombre aléatoire entre 1 et 10
```

Vous pouvez également générer des nombres aléatoires dans un intervalle spécifique en utilisant `random(in:)` avec des valeurs personnalisées:

```
let randomNum = Double.random(in: 0.5...1)
print(randomNum) //output: un nombre aléatoire entre 0.5 et 1
```

## Plongée en profondeur:
La génération de nombres aléatoires remonte aux premiers ordinateurs, où les mathématiciens utilisaient des techniques comme la division et la multiplication pour créer des nombres aléatoires. Aujourd'hui, la plupart des langages de programmation ont leur propre fonction de génération de nombres aléatoires intégrée, comme Swift avec `random(in:)`.

Il existe également d'autres moyens de générer des nombres aléatoires en utilisant des algorithmes mathématiques sophistiqués ou même des capteurs physiques comme la radioactivité. Cependant, ils sont rarement utilisés dans les applications courantes car les fonctions intégrées sont plus que suffisantes.

## Voir aussi:
Pour en savoir plus sur la génération de nombres aléatoires en Swift, vous pouvez consulter la documentation officielle d'Apple sur les nombres aléatoires: [Random Number Generation](https://developer.apple.com/documentation/swift/random).

Vous pouvez également trouver des ressources utiles pour optimiser vos fonctions de génération de nombres aléatoires sur le blog de NSHipster: [Randomization in Swift](https://nshipster.com/random/).