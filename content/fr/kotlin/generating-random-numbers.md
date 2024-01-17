---
title:                "Génération de nombres aléatoires"
html_title:           "Kotlin: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Génération de nombres aléatoires en Kotlin

## Qu'est-ce que c'est et pourquoi est-ce important?
La génération de nombres aléatoires est une technique utilisée par les programmeurs pour générer des valeurs aléatoires dans un programme. Cela peut être utile pour créer des jeux, des simulations ou pour ajouter une certaine imprévisibilité dans un programme.

## Comment faire:
Voici un exemple de code en Kotlin pour générer un nombre aléatoire entre 1 et 10 et l'afficher dans la console:
```Kotlin
val random = (1..10).random()
println(random)
```
La sortie sera un nombre aléatoire compris entre 1 et 10, tel que 4 ou 8.

## Plongée en profondeur:
La génération de nombres aléatoires a une longue histoire en informatique. Au début, les programmes utilisaient des algorithmes déterministes pour générer des nombres pseudo-aléatoires, mais cela a été remplacé par des générateurs de nombres réellement aléatoires, tels que les générateurs basés sur le bruit thermique.

Bien qu'en Kotlin, nous puissions utiliser la méthode `random()` pour générer des nombres aléatoires, il existe également d'autres alternatives telles que la bibliothèque `java.util.Random`. De plus, il est important de comprendre que les nombres générés par les ordinateurs ne sont pas vraiment aléatoires, mais qu'ils sont calculés en utilisant des algorithmes.

## Voir aussi:
Pour en savoir plus sur la génération de nombres aléatoires en Kotlin, voici quelques sources utiles:
- La documentation officielle de Kotlin sur les nombres aléatoires: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/
- Un tutoriel sur la génération de nombres aléatoires en Kotlin: https://www.baeldung.com/kotlin/random-numbers
- Une explication plus détaillée sur la différence entre nombres pseudo-aléatoires et réellement aléatoires: https://www.geeksforgeeks.org/pseudo-random-vs-true-random-number-generators/