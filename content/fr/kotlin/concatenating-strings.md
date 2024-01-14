---
title:                "Kotlin: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des compétences de base en programmation est la manipulation de chaînes de caractères. Que ce soit pour afficher du texte à l'écran, créer des messages personnalisés ou même stocker des informations, les chaînes de caractères sont un élément essentiel de tout programme. Une manière de manipuler efficacement les chaînes de caractères est la concaténation, qui consiste à combiner plusieurs chaînes en une seule. Dans cet article, nous allons en apprendre plus sur la concatenation de strings en Kotlin et pourquoi c'est important pour les programmeurs.

## Comment faire

En utilisant Kotlin, la concatenation de strings est très simple. Tout d'abord, nous allons définir deux chaînes de caractères séparées avec un signe plus, comme ceci :

```Kotlin
val firstName = "Jean-Philippe"
val lastName = "Martin"
```

Ensuite, pour les concaténer, nous allons simplement utiliser le signe plus pour les combiner en une seule chaîne, comme ceci :

```Kotlin
val fullName = firstName + lastName
```

Maintenant, si nous voulons afficher cette valeur, nous pouvons utiliser la fonction `println()` avec la variable `fullName` comme paramètre :

```Kotlin
println(fullName)
```

Ce qui va afficher `Jean-PhilippeMartin` à l'écran. Nous pouvons également concaténer des valeurs de types différents, comme ceci :

```Kotlin
val age = 30
println("Mon âge est : " + age)
```

Ce qui va afficher `Mon âge est : 30` à l'écran.

## Plongée en profondeur

En plus de la concatenation simple avec le signe plus, Kotlin propose également une méthode appelée `plus()` pour combiner des chaînes de caractères. Elle peut être utilisée ainsi :

```Kotlin
val firstName = "Jean-Philippe"
val lastName = "Martin"
val fullName = firstName.plus(lastName)
```

Et le résultat sera le même qu'avec l'opérateur plus. De plus, Kotlin offre également la possibilité d'utiliser des chaînes de formatage avec la fonction `format()`. Cela permet de créer des chaînes dynamiques en insérant des valeurs à partir de variables :

```Kotlin
val age = 30
val fullName = "Jean-Philippe Martin"
val message = "Bonjour, je m'appelle %s et j'ai %d ans".format(fullName, age)
println(message)
```

Ce qui va afficher `Bonjour, je m'appelle Jean-Philippe Martin et j'ai 30 ans` à l'écran.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes de caractères en Kotlin, voici quelques liens utiles :

- [Documentation officielle sur la classe String de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Tutoriel sur la concatenation de strings en Kotlin](https://www.programiz.com/kotlin-programming/strings)
- [Vidéo explicative sur la concatenation en Kotlin](https://www.youtube.com/watch?v=bwjVEjoEPDA)

Maintenant que vous savez comment concaténer des chaînes de caractères en Kotlin, vous pouvez utiliser cette compétence dans vos programmes pour créer des chaînes dynamiques et personnalisées. N'oubliez pas de pratiquer et d'explorer d'autres fonctionnalités de manipulation de chaînes en Kotlin pour devenir un·e expert·e en la matière. Bon codage !