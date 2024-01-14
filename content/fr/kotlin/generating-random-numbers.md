---
title:    "Kotlin: Génération de nombres aléatoires"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être entendu parler des nombres aléatoires en programmation, mais vous vous demandez peut-être pourquoi ils sont importants. En fait, générer des nombres aléatoires peut être utile dans de nombreuses situations différentes. Par exemple, vous pourriez avoir besoin de créer un jeu vidéo où les obstacles doivent apparaître de manière aléatoire ou vous pourriez vouloir effectuer des tests avec des données aléatoires. Dans les deux cas, la génération de nombres aléatoires est cruciale pour obtenir des résultats variés et réalistes.

## Comment faire

Il existe plusieurs façons de générer des nombres aléatoires en utilisant Kotlin. L'une des façons les plus courantes est d'utiliser la fonction `random()` de la bibliothèque standard de Kotlin. Cette fonction renvoie un nombre aléatoire entre 0 et 1. Vous pouvez ensuite multiplier le résultat par le nombre maximum souhaité et ajouter le nombre minimum pour obtenir un nombre aléatoire compris dans une plage spécifique.

```Kotlin
val randomNumber = (0..10).random() // génère un nombre aléatoire compris entre 0 et 10
```

Si vous voulez une valeur aléatoire de type `Double`, vous pouvez utiliser la fonction `nextDouble()` de la classe `Random`. Cette méthode renvoie un nombre aléatoire entre 0.0 et 1.0. Vous pouvez ensuite le multiplier ou le diviser selon vos besoins.

```Kotlin
val randomNumber = Random().nextDouble() * 100 // génère un nombre aléatoire entre 0.0 et 100.0
```

Il est également possible de générer un nombre aléatoire dans une plage spécifique à l'aide de la méthode `nextInt()` de la classe `Random`. Cette méthode accepte un nombre maximal en tant que paramètre et renvoie un nombre aléatoire compris entre 0 (inclus) et le nombre maximal fourni (exclus).

```Kotlin
val randomNumber = Random().nextInt(50) // génère un nombre aléatoire entre 0 et 49
```

## Plongée en profondeur

Les nombres aléatoires générés par les méthodes décrites ci-dessus peuvent sembler vraiment aléatoires, mais en réalité, ils sont basés sur une séquence prédéfinie appelée "seed". Cette séquence est générée par l'algorithme de génération de nombres aléatoires utilisé par Kotlin. Par défaut, la seed est basée sur l'heure actuelle, mais vous pouvez également la définir manuellement en utilisant la méthode `setSeed()` de la classe `Random`.

Il est également important de noter que les nombres aléatoires générés par les méthodes mentionnées peuvent se répéter si elles sont utilisées plusieurs fois, car la seed est toujours la même. Pour éviter cela, vous pouvez utiliser un objet `Random` partagé pour générer vos nombres aléatoires, au lieu d'en créer un nouveau à chaque fois.

## Voir aussi

- [Documentation de la bibliothèque standard de Kotlin - Fonction `random()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/random.html)
- [Documentation de la classe `Random` de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Guide de génération de nombres aléatoires en Kotlin](https://www.tutorialkart.com/kotlin/generating-random-numbers-in-kotlin/)