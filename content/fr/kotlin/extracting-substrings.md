---
title:                "Kotlin: Extraction de sous-chaînes"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi utiliser l'extraction de sous-chaînes en Kotlin ?

Si vous travaillez avec des chaînes de caractères en Kotlin, vous savez sûrement déjà que parfois vous n'avez besoin que d'une partie de cette chaîne. C'est là qu'entre en jeu l'extraction de sous-chaînes. Cette technique vous permet de récupérer uniquement la partie de la chaîne dont vous avez besoin, ce qui peut être très utile dans de nombreuses situations.

## Comment faire l'extraction de sous-chaînes en Kotlin

Pour extraire une sous-chaîne en Kotlin, vous pouvez utiliser la fonction `substring` sur un objet de type chaîne de caractères. Cette fonction prend en paramètres l'indice de début et l'indice de fin de la sous-chaîne que vous souhaitez extraire. Par exemple :

```Kotlin
val chaine = "Bonjour tout le monde !"
val sousChaine = chaine.substring(8, 12)
println(sousChaine) // Affiche "tout"
```

Vous pouvez également utiliser la méthode `subSequence` pour extraire une sous-chaîne en spécifiant les mêmes paramètres, mais en utilisant une syntaxe différente :

```Kotlin
val chaine = "Bonjour tout le monde !"
val sousChaine = chaine.subSequence(13, 19)
println(sousChaine) // Affiche "monde !"
```

Il est également possible de spécifier un seul indice pour extraire une sous-chaîne à partir de cet élément jusqu'à la fin de la chaîne :

```Kotlin
val chaine = "Bonjour tout le monde !"
val sousChaine = chaine.substring(8)
println(sousChaine) // Affiche "tout le monde !"
```

## Plongeons plus profondément dans l'extraction de sous-chaînes

Maintenant que vous avez vu comment utiliser la fonction `substring`, il est important de comprendre comment les indices fonctionnent. En Kotlin, les indices d'une chaîne de caractères commencent à 0. Cela signifie que le premier caractère de la chaîne a un indice de 0, le deuxième un indice de 1, et ainsi de suite.

Il est également important de noter que l'indice de fin de la sous-chaîne n'est pas inclus dans la sous-chaîne extraite. Cela signifie que si vous spécifiez un indice de 8 à 12, la sous-chaîne extraite comprendra les caractères 8, 9, 10 et 11, mais pas 12.

Vous pouvez également utiliser des indices négatifs pour extraire une sous-chaîne en partant de la fin de la chaîne. Par exemple :

```Kotlin
val chaine = "Bonjour tout le monde !"
val sousChaine = chaine.substring(8, -8)
println(sousChaine) // Affiche "tout le"
```

Dans cet exemple, l'indice de fin de la sous-chaîne est négatif, ce qui signifie qu'il est décalé de 8 caractères à partir de la fin de la chaîne. Ainsi, la sous-chaîne extraite comprend les caractères de "tout" jusqu'à "me".

# Voir aussi

- [Tutoriel Kotlin - Working with Strings](https://kodejava.org/kotlin-working-with-strings-examples/)
- [Documentation Kotlin - Manipuler les chaînes de caractères](https://kotlinlang.org/docs/reference/basic-types.html#strings)