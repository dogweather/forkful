---
title:                "Kotlin: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation de chaînes de caractères est une tâche simple mais parfois nécessaire lors de la manipulation de données. Elle peut être utile lorsque vous souhaitez normaliser le format des noms ou titres, ou encore pour faciliter les comparaisons entre différentes chaînes.

## Comment

Il existe différentes façons de capitaliser une chaîne de caractères en Kotlin. Voici deux options couramment utilisées :

```
Kotlin

// Option 1 : Utiliser la fonction capitalize()
val message = "bonjour le monde"
val capitalized = message.capitalize()
println(capitalized) // Bonjour le monde

// Option 2 : Utiliser la fonction toUpperCase() avec substring()
val message = "bonjour le monde"
val capitalized = message.substring(0, 1).toUpperCase() + message.substring(1)
println(capitalized) // Bonjour le monde
```

## Plongée en profondeur

En utilisant la première option, la fonction capitalize() utilise la règle de capitalisation de la langue par défaut du système, qui est généralement basée sur les règles du langage anglais. Cela signifie qu'elle va mettre en majuscule seulement la première lettre de la chaîne et laisser les autres lettres telles qu'elles étaient.

Quant à la deuxième option, la fonction toUpperCase() transforme toutes les lettres de la chaîne en majuscules, et en utilisant la fonction substring(), nous pouvons spécifier à quelles parties de la chaîne nous souhaitons appliquer cette transformation.

Il est également important de noter que ces fonctions ne modifient pas la chaîne d'origine, mais retournent une nouvelle chaîne avec la capitalisation souhaitée.

## Voir aussi

- Documentation officielle de Kotlin sur la manipulation de chaînes de caractères : https://kotlinlang.org/docs/reference/basic-types.html#string
- Article sur la manipulation de chaînes de caractères en Kotlin : https://medium.com/@abeythilake/java-vs-kotlin-string-transformation-f135815b0cec