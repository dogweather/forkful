---
title:    "Kotlin: Génération de nombres aléatoires"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi 
Avez-vous déjà eu besoin de générer des nombres aléatoires dans vos programmes Kotlin ? Peut-être pour simuler un jeu de hasard ou pour générer des données aléatoires pour des tests. Dans cet article, nous allons explorer comment générer des nombres aléatoires en utilisant Kotlin.

# Comment faire 
Générer des nombres aléatoires en Kotlin est assez facile grâce à la classe `Random` de Kotlin. Tout d'abord, vous devez l'importer dans votre code en utilisant `import kotlin.random.Random`.

Ensuite, vous pouvez utiliser différentes méthodes pour générer les nombres aléatoires en fonction de vos besoins. Par exemple, pour générer un nombre entier aléatoire dans une certaine plage, vous pouvez utiliser `nextInt()` en spécifiant les bornes supérieure et inférieure comme paramètres : 

```Kotlin 
val randomNumber = Random.nextInt(1, 11)
```
Cela générera un nombre aléatoire entre 1 et 10 inclus.

Si vous avez besoin d'un nombre à virgule aléatoire, vous pouvez utiliser `nextDouble()` en spécifiant les bornes comme paramètres. Par exemple : 

```Kotlin 
val randomNumber = Random.nextDouble(0.0, 1.0)
```
Cela générera un nombre à virgule aléatoire entre 0.0 et 1.0.

Vous pouvez également utiliser des méthodes telles que `nextBoolean()` pour générer des valeurs booléennes aléatoires et `nextBytes()` pour générer des tableaux de bytes aléatoires.

# Plongée en profondeur 
La classe `Random` utilise en fait un algorithme appelé PRNG (Pseudo-Random Number Generator) pour générer les nombres aléatoires. Cela signifie que les valeurs générées ne sont pas réellement aléatoires, mais elles semblent aléatoires en fonction des algorithmes utilisés.

Il est important de noter que si vous utilisez la même instance de `Random` plusieurs fois, vous obtiendrez les mêmes valeurs aléatoires à chaque fois. C'est parce que l'algorithme est basé sur une "graine" qui est utilisée pour générer les nombres. Si vous voulez générer des valeurs différentes à chaque utilisation, vous pouvez créer une nouvelle instance `Random` à chaque fois.

# Voir aussi 
- [Documentation officielle de Kotlin sur la classe `Random`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Article sur la génération de nombres aléatoires en Java](https://dzone.com/articles/random-number-generation-in-java)
- [Documentation sur les PRNG](https://en.wikipedia.org/wiki/Pseudo-random_number_generator)