---
title:                "Kotlin: Concaténation de chaînes de caractères"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une tâche courante dans la programmation Kotlin. Cela permet de combiner différentes chaînes de caractères pour former une nouvelle chaîne qui contient toutes les informations nécessaires. Cela peut être utile pour afficher des messages à l'utilisateur, pour l'assemblage de documents dynamiques ou pour une variété d'autres tâches de manipulation de textes.

## Comment faire

Pour concaténer des chaînes de caractères en Kotlin, vous pouvez utiliser l'opérateur `+`, les fonctions `plus()` ou `plusAssign()`, ou encore la méthode `concat()`.

```Kotlin
// Utilisation de l'opérateur +
val a = "Bonjour"
val b = "tout le monde"
val c = a + " " + b // c = "Bonjour tout le monde"

// Utilisation de la fonction plus()
val x = "Hello"
val y = "world"
val z = x.plus(" ").plus(y) // z = "Hello world"

// Utilisation de la fonction plusAssign()
var msg = "Bonjour"
msg += " tout le monde" // msg = "Bonjour tout le monde"

// Utilisation de la méthode concat()
val firstName = "John"
val lastName = "Doe"
val fullName = firstName.concat(" ", lastName) // fullName = "John Doe"
```

L'utilisation de l'une de ces méthodes produit le même résultat : une nouvelle chaîne de caractères contenant la concaténation des chaînes d'origine.

## Deep Dive

Il est important de noter que, bien que la concaténation de chaînes de caractères soit une opération utile, elle peut être gourmande en termes de performances si elle est utilisée de manière intensive. Cela est dû au fait que chaque fois qu'une nouvelle chaîne est créée, la mémoire doit être allouée pour stocker la nouvelle chaîne et les deux chaînes d'origine doivent être copiées.

Pour éviter des problèmes de performances, vous pouvez utiliser la classe `StringBuilder` qui permet de concaténer efficacement plusieurs chaînes de caractères sans les copier à chaque fois.

```Kotlin
val builder = StringBuilder()
builder.append("Bonjour")
builder.append(" tout le monde")
val result = builder.toString() // result = "Bonjour tout le monde"
```

L'utilisation de `StringBuilder` peut grandement améliorer les performances lors de la concaténation de chaînes de caractères, en particulier si vous travaillez avec de grandes quantités de données.

## Voir aussi

- [Documentation Kotlin sur la concaténation de chaînes de caractères](https://kotlinlang.org/docs/basic-types.html#strings)
- [Article sur la classe `StringBuilder` en Kotlin](https://www.baeldung.com/kotlin/stringbuilder)
- [Vidéo sur les performances de la concaténation de chaînes en Kotlin](https://www.youtube.com/watch?v=diUcJ5nwHkU)