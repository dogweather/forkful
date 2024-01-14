---
title:    "Kotlin: Concaténation de chaînes de caractères"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante en programmation qui permet de combiner plusieurs chaînes pour en former une seule. Cela peut être utile dans de nombreuses situations, notamment pour afficher du texte à l'écran ou pour créer des URL.

## Comment faire

Pour concaténer des chaînes de caractères en Kotlin, il suffit d'utiliser l'opérateur `+` entre les différentes chaînes. Par exemple :

```Kotlin
val prenom = "Jean"
val nom = "Dupont"
val nomComplet = prenom + nom
```

Dans cet exemple, la variable `nomComplet` contiendra la valeur "JeanDupont".

Il est également possible d'utiliser la méthode `plus()` pour concaténer des chaînes, comme ceci :

```Kotlin
val age = 30
val message = "J'ai " plus age plus " ans."
```

La variable `message` contiendra alors la valeur "J'ai 30 ans.".

## Approfondissement

Lorsqu'on utilise l'opérateur `+` pour concaténer des chaînes, Kotlin utilise en réalité la méthode `plus()` en arrière-plan. Cette méthode est définie dans la classe `String` et permet de concaténer deux chaînes en les combinant dans un nouvel objet `String`.

Il est également important de noter que la concaténation de chaînes peut être gourmande en ressources, notamment lorsque l'on concatène un grand nombre de chaînes. Dans ces cas-là, il est préférable d'utiliser la classe `StringBuilder` qui offre de meilleures performances pour la concaténation de chaînes.

## Voir aussi

- [Documentation officielle de Kotlin sur la concaténation de chaînes](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Explications détaillées sur l'utilisation de la classe StringBuilder en Kotlin](https://kotlinlang.org/docs/basic-types.html#string-representation)
- [Exemples pratiques de concaténation de chaînes en Kotlin](https://blog.kotlin-academy.com/kotlin-string-concatenation-performance-d4cf67d18f1b)