---
title:    "Kotlin: Concaténation de chaînes"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une technique fréquemment utilisée en programmation permettant de combiner plusieurs chaînes de caractères en une seule. Elle est particulièrement utile pour afficher des messages personnalisés ou pour construire des requêtes dynamiques. Dans cet article, nous allons voir comment concaténer des chaînes de caractères en Kotlin de manière efficace et optimale.

## Comment faire

Pour concaténer des chaînes de caractères en Kotlin, nous pouvons utiliser l'opérateur plus (+) ou bien la fonction `plus()`.

Les exemples suivants illustrent ces deux méthodes :

```Kotlin
var message = "Bonjour "
val name = "Marie"
message = message + name
println(message)
```

Output:
```
Bonjour Marie
```

```Kotlin
val firstName = "Jean"
val lastName = "Dupont"
val fullName = firstName.plus(lastName)
println(fullName)
```

Output:
```
JeanDupont
```

Nous pouvons également utiliser la fonction `format()` pour concaténer des chaînes de caractères en incluant des variables à l'intérieur. L'exemple suivant montre comment utiliser cette méthode :

```Kotlin
val year = 2021
val message = "Nous sommes en %d".format(year)
println(message)
```

Output: 
```
Nous sommes en 2021
```

L'utilisation de la fonction `format()` est particulièrement pratique lorsque nous devons concaténer plusieurs variables dans une chaîne de caractères.

Notez que le symbole de pourcentage (%) doit être placé à l'endroit où nous voulons insérer la variable, et que le type de la variable doit être adapté. Par exemple, si nous voulons insérer un entier (%d), nous devons fournir une variable de type Int.

## Deep Dive

En Kotlin, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Ainsi, à chaque fois que nous concaténons des chaînes de caractères en utilisant l'opérateur plus (+) ou la fonction `plus()`, une nouvelle chaîne de caractères est créée et l'ancienne est supprimée.

Pour éviter cette surconsommation de ressources, nous pouvons utiliser la classe `StringBuilder` qui permet de modifier une chaîne de caractères sans créer de nouvelles instances.

L'exemple suivant montre comment concaténer des chaînes de caractères en utilisant la classe `StringBuilder` :

```Kotlin
val sb = StringBuilder("Bonjour")
val name = "Marie"
val message = sb.append(name).append("!").toString()
println(message)
```

Output:
```
Bonjour Marie!
```

En utilisant `StringBuilder`, nous avons créé une seule instance de chaîne de caractères et l'avons modifiée à l'aide de la méthode `append()`. Cela permet d'économiser des ressources et d'améliorer les performances de notre code.

## Voir aussi

- [Documentation officielle de Kotlin sur les chaînes de caractères](https://kotlinlang.org/docs/basic-types.html#strings)
- [Différences entre les opérateurs et les fonctions en Kotlin](https://www.baeldung.com/kotlin-operators-functions)