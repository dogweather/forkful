---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
La concaténation des chaînes est le mécanisme d'assemblage de deux ou de plusieurs chaînes en une seule. Les programmeurs l'utilisent pour combiner des valeurs de chaînes ou pour créer des messages formatés pour l'affichage.

## Comment faire :
Voici comment procéder en Kotlin :

```Kotlin
fun main(args: Array<String>) {
    val str1 = "Bonjour, "
    val str2 = "le monde!"
    val message = str1 + str2
    println(message)
}
```

Résultat :
```
Bonjour, le monde!
```

Utilisation de la méthode `plus` pour la concaténation des chaînes :

```Kotlin
fun main(args: Array<String>) {
    val str1 = "Bonjour, "
    val str2 = "le monde!"
    val message = str1.plus(str2)
    println(message)
}
```

Résultat :
```
Bonjour, le monde!
```

## Approfondissement :

Dans le passé, les programmeurs utilisaient l'opérateur `+` pour concaténer des chaînes, comme nous le faisons en Java. Cependant, Kotlin a introduit la méthode `plus` qui est plus efficace et plus lisible. 

Si vous devez concaténer plusieurs chaînes, il est préférable d'utiliser `StringBuilder`. Il est plus performant car il évite la création de nombreux objets String intermédiaires.

```Kotlin
fun main(args: Array<String>) {
    val str1 = StringBuilder("Bonjour, ")
    val str2 = "le monde!"
    str1.append(str2)
    println(str1.toString())
}
```

Résultat :
```
Bonjour, le monde!
```

## Voir aussi :

Pour plus d'informations, consultez les ressources suivantes :
- La documentation officielle de Kotlin : [Working with Strings](https://kotlinlang.org/docs/reference/strings.html)
- Le guide de la concaténation des chaînes en Kotlin : [Kotlin String concatenation](https://www.baeldung.com/kotlin-string-concatenation)
- Autres méthodes de manipulation des chaînes en Kotlin : [String Operations](https://www.programiz.com/kotlin-programming/string-operations)