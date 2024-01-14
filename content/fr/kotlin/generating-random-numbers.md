---
title:                "Kotlin: Génération de nombres aléatoires"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Générer des nombres aléatoires est une opération courante dans la programmation, utile pour simuler des situations aléatoires ou pour créer des jeux et des applications divertissantes. En utilisant Kotlin, nous pouvons facilement générer des nombres aléatoires et les incorporer dans nos projets.

## Comment faire

Tout d'abord, nous devons importer la classe Random de la bibliothèque standard de Kotlin pour pouvoir générer des nombres aléatoires. Ensuite, nous pouvons utiliser la méthode nextInt() pour générer un nombre entier aléatoire dans une plage donnée. Par exemple, si nous voulons générer un nombre entre 1 et 10 inclus, nous pouvons utiliser la syntaxe suivante :
```
Kotlin
val randomNumber = Random().nextInt(10) + 1
```
Nous pouvons également utiliser la méthode nextDouble() pour générer un nombre à virgule aléatoire entre 0 et 1. Par exemple :
```
Kotlin
val randomDecimal = Random().nextDouble()
```
Enfin, pour générer des booléens aléatoires, nous pouvons utiliser la méthode nextBoolean(). Par exemple :
```
Kotlin
val randomBoolean = Random().nextBoolean()
```

## Plongée profonde

La classe Random de Kotlin utilise un algorithme appelé "Mersenne Twister" pour générer des nombres aléatoires. Cet algorithme a une période extrêmement longue, ce qui signifie qu'il peut générer une séquence apparemment aléatoire sans se répéter pendant un certain temps. Cependant, comme les nombres sont générés informatiquement, ils ne sont pas vraiment aléatoires, mais seulement pseudo-aléatoires.

Pour garantir que les nombres générés sont aussi aléatoires que possible, il est important d'utiliser une graine (seed) différente à chaque fois que la classe Random est instanciée. Une mauvaise graine peut entraîner la génération de nombres prévisibles et donc une perte de la caractéristique aléatoire.

## Voir aussi
- [Documentation de la classe Random en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random)
- [Article sur les nombres aléatoires en programmation](https://www.geeksforgeeks.org/random-number-generation-in-kotlin/) (en anglais)
- [Exemples de jeux utilisant la génération de nombres aléatoires en Kotlin](https://github.com/tonkopetkov/KotlinRandomGameExamples) (en anglais)