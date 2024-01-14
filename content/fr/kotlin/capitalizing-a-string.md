---
title:    "Kotlin: Majuscule d'une chaîne de caractères"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Capitaliser une chaîne de caractères est une tâche commune en programmation. Cela peut être utile pour mettre en évidence certains mots ou pour suivre les conventions de nommage dans votre code. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant le langage de programmation Kotlin.

## Comment faire

Tout d'abord, nous allons créer une chaîne de caractères en utilisant l'opérateur de concaténation "+" :

```Kotlin
var phrase = "je suis un programmeur kotlin"
```

Ensuite, nous allons utiliser la fonction .toUpperCase() pour capitaliser la première lettre de chaque mot dans la chaîne :

```Kotlin
var phraseCapitale = phrase.split(' ').joinToString(" ") { it.capitalize() }
```

Dans cet exemple, nous avons utilisé la fonction .capitalize() pour capitaliser la première lettre de chaque mot dans notre chaîne. Ensuite, nous avons utilisé la fonction .joinToString() pour rejoindre les mots capitalisés en une seule chaîne, en utilisant l'espace comme délimiteur.

Enfin, pour afficher le résultat, nous allons simplement imprimer la variable phraseCapitale :

```Kotlin
println(phraseCapitale)
```

Lorsque nous exécutons ce code, nous obtenons la sortie suivante :

```Kotlin
Je Suis Un Programmeur Kotlin
```

## Plongée en profondeur

En utilisant la fonction .capitalize() dans notre exemple, nous avons capitalisé la première lettre de chaque mot. Cependant, il est important de noter que cette fonction ne prend pas en compte les caractères spéciaux ou les nombres.

Pour capitaliser une chaîne de caractères en tenant compte de ces éléments, nous pouvons utiliser la fonction .capitalize() avec .toUpperCase() :

```Kotlin
var phraseCapitale = phrase.split(' ').joinToString(" ") { it.toLowerCase().capitalize() }
```

En utilisant .toLowerCase() avant .capitalize(), nous nous assurons que tous les caractères sont en minuscules avant d'en capitaliser la première lettre. De cette façon, même les mots avec des caractères spéciaux ou des chiffres seront correctement capitalisés.

## Voir aussi

- [Documentation officielle Kotlin](https://kotlinlang.org/docs/reference/strings.html#accessing-parts-of-a-string)
- [Comment capitaliser une chaîne en Kotlin](https://kotlinlang.org/docs/reference/extensions.html#scala-like-string-manipulation)
- [Différentes façons de manipuler les chaînes en Kotlin](https://medium.com/@napperley/android-studio-different-ways-to-manipulate-strings-in-kotlin-f2d0148b4f9b)