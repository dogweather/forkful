---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Kotlin: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Kotlin, vous serez sans doute confrontés à la tâche de capitaliser une chaîne de caractères (c'est-à-dire mettre la première lettre de chaque mot en majuscule). Dans cet article, nous allons découvrir pourquoi cela peut être utile et comment le faire en utilisant Kotlin.

## Comment faire

Pour capitaliser une chaîne de caractères en Kotlin, nous allons utiliser la fonction `capitalize()`. Vous pouvez l'utiliser de la manière suivante :

```Kotlin
val str = "exemple de chaîne"
val strCapitalize = str.capitalize()

println(strCapitalize) // Exemple de chaîne
```

Comme vous pouvez le voir, la méthode `capitalize()` retourne une nouvelle chaîne avec la première lettre en majuscule.

Si vous souhaitez capitaliser uniquement la première lettre de la première partie de la chaîne, vous pouvez utiliser la fonction `capitalizeFirst()` de l'extension `StringKt`. Voici un exemple :

```Kotlin
val str = "exemple de chaîne"
val strCapitalizeFirst = str.capitalizeFirst()

println(strCapitalizeFirst) // Exemple de chaîne
```

De plus, si vous voulez capitaliser tous les mots d'une chaîne, vous pouvez utiliser la fonction `capitalizeEachWord()` de l'extension `StringKt`. Voici un exemple :

```Kotlin
val str = "exemple de chaîne"
val strCapitalizeEachWord = str.capitalizeEachWord()

println(strCapitalizeEachWord) // Exemple De Chaîne
```

## Plongée en profondeur

La fonction `capitalize()` utilise le paramètre `locale` pour déterminer quelles lettres doivent être mises en majuscule. Par défaut, cela dépend de la configuration de votre appareil. Si vous souhaitez spécifier une langue en particulier, vous pouvez utiliser la surcharge de la fonction avec le paramètre `locale`.

En outre, si vous voulez capitaliser une chaîne avec des règles spécifiques, vous pouvez utiliser la fonction `capitalizeBy()` de l'extension `StringKt`. Elle prend en paramètre un objet `CharTransformer` qui définit les règles de capitalisation à appliquer. Voici un exemple :

```Kotlin
val str = "exemple de chaîne"
val customTransformer = object : CharTransformer {
    override fun transform(c: Char): Char {
        // Ici, nous changeons la première lettre en majuscule et la deuxième en minuscule
        return if (c == str[0]) c.toUpperCase() else c.toLowerCase()
    }
}

val strCapitalizeCustom = str.capitalizeBy(customTransformer)

println(strCapitalizeCustom) // Exemple De Chaîne
```

## Voir aussi

- [La documentation officielle de Kotlin sur la fonction `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [La documentation officielle de Kotlin sur les extensions de chaînes de caractères](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html#string-extensions)