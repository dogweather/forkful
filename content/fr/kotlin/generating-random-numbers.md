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

## Pourquoi

Vous vous demandez peut-être pourquoi générer des nombres aléatoires est utile dans la programmation. Eh bien, c'est une tâche courante dans la création de jeux, d'applications de loterie ou de tests de logiciel.

## Comment faire

Générer des nombres aléatoires en utilisant Kotlin est très simple. Il suffit d'utiliser la fonction `random()` de la bibliothèque standard de Kotlin. Voici un exemple de code qui génère un nombre aléatoire entre 1 et 10 et l'affiche :

```Kotlin
val randomNum = (1..10).random()
println(randomNum)
```

Pour générer un nombre aléatoire avec une limite supérieure spécifique, vous pouvez utiliser la fonction `nextInt()` et lui donner la limite en paramètre. Par exemple, pour générer un nombre aléatoire entre 1 et 100 :

```Kotlin
val randomNum = Random.nextInt(100) + 1
println(randomNum)
```

Vous pouvez également utiliser la fonction `nextDouble()` pour générer un nombre aléatoire entre 0 et 1 avec des décimales.

## Plongée en profondeur

En utilisant la fonction `random()` pour générer des nombres aléatoires, il est important de noter qu'elle utilise la classe `Random` de Java sous-jacente. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais plutôt pseudo-aléatoires. Cela signifie que si les mêmes paramètres sont utilisés pour générer des nombres aléatoires à partir de la même graine, les résultats seront toujours les mêmes. Pour éviter cela, vous pouvez spécifier une graine différente à l'aide de la fonction `setSeed()` avant de générer des nombres aléatoires.

## Voir aussi

Pour en savoir plus sur la génération de nombres aléatoires en Kotlin, vous pouvez consulter les liens suivants :

- Documentation officielle de Kotlin sur la bibliothèque standard : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/
- Tutoriel de Baeldung sur la génération de nombres aléatoires en Kotlin : https://www.baeldung.com/kotlin-random
- Article de Baeldung sur la génération de nombres aléatoires sans répétition en Kotlin : https://www.baeldung.com/kotlin-random-no-repeats