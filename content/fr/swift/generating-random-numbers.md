---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La génération de nombres aléatoires est une fonction essentielle en programmation. Elle sert à créer de la diversité et de l'incertitude simulée, permettant d'évaluer des scénarios variés, de créer des éléments de jeu imprévisibles, etc.

## Comment faire:

En Swift, il est très facile de générer des nombres aléatoires. Vous pouvez utiliser la fonction `random(in:)` avec la gamme de nombres désirée. Voici un simple exemple.

```Swift
let randomInt = Int.random(in: 1..<100)
print(randomInt)
```

Ce code générera un entier aléatoire compris entre 1 et 99. Chaque exécution du code donnera un résultat différent.

## Plongée profonde

Historiquement, la génération de nombres aléatoires a longtemps été un sujet délicat et crucial en informatique. Plus tôt, les langages de programmation utilisaient des algorithmes simples, mais prévisibles et donc potentiellement manipulables, pour générer des nombres aléatoires.

Swift utilise maintenant un générateur de nombres aléatoires cryptographiquement sécurisé. C'est un grand pas en avant pour la sécurité et la fiabilité.

Il y a aussi d'autres façons de générer des nombres aléatoires en Swift, en utilisant des bibliothèques tierces ou des approches plus complexes. Par exemple, la bibliothèque GameplayKit a une suite de générateurs de nombres aléatoires avec différentes propriétés.

En termes de mise en œuvre, la fonction `random(in:)` utilise le générateur de nombres aléatoires par défaut du système, à moins que vous ne fournissiez le vôtre.

## Voir aussi

Pour approfondir le sujet de la génération de nombres aléatoires en Swift, consultez ces liens :

- [Documentation officielle de Swift sur les nombres aléatoires](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- [Un guide complet pour générer des nombres aléatoires en Swift](https://www.hackingwithswift.com/articles/141/how-to-generate-random-numbers-in-swift)
- [Fonction random de la bibliothèque GameplayKit](https://developer.apple.com/documentation/gameplaykit/gkrandomdistribution)