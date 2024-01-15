---
title:                "Concaténation de chaînes"
html_title:           "Kotlin: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Kotlin, vous rencontrerez certainement un jour la nécessité de concaténer des chaînes de caractères. Cela peut sembler anodin, mais c'est une compétence de base importante à maîtriser pour tout programmeur.

## Comment procéder

Cela peut sembler évident, mais pour concaténer des chaînes de caractères en Kotlin, vous pouvez les utiliser à l'aide de l'opérateur `+`.

```Kotlin
val firstName = "Marie"
val lastName = "Dupont"
val fullName = firstName + " " + lastName  // Résultat : "Marie Dupont"
```

Vous pouvez également utiliser la méthode `plus()` pour concaténer plusieurs chaînes.

```Kotlin
val city = "Paris"
val country = "France"
val location = city.plus(", ").plus(country)  // Résultat : "Paris, France"
```

Il est également possible de concaténer des chaînes avec des variables de type numérique ou d'autres objets. Dans ce cas, vous devrez utiliser la méthode `toString()` pour convertir l'objet en chaîne de caractères avant de le concaténer.

```Kotlin
val age = 35
val message = "J'ai " + age.toString() + " ans."  // Résultat : "J'ai 35 ans."
```

Il existe une autre méthode plus efficace pour concaténer plusieurs chaînes de caractères : `StringBuilder`. Cette méthode est recommandée car elle est plus performante en termes de mémoire et de temps d'exécution lors de la concaténation de plusieurs chaînes.

```Kotlin
val firstName = "Jean"
val lastName = "Dupont"
val fullName = StringBuilder().append(firstName)
                               .append(" ")
                               .append(lastName).toString()  // Résultat : "Jean Dupont"
```

## Plongée en profondeur

En Kotlin, les chaînes de caractères sont des objets immuables, ce qui signifie qu'une fois créées, elles ne peuvent pas être modifiées. Donc, chaque fois que vous utilisez les opérateurs `+` ou `plus()` pour concaténer des chaînes, un nouvel objet de chaîne est créé pour contenir la valeur concaténée. Cela peut causer des problèmes de performances si vous concaténez un grand nombre de chaînes de caractères.

C'est là que `StringBuilder` intervient. Cette classe permet de créer un objet modifiable où vous pouvez ajouter de nouvelles chaînes de caractères sans créer d'objets supplémentaires. Cela peut être très utile si vous devez concaténer de grandes quantités de chaînes de caractères ou si vous devez le faire de manière répétée dans une boucle.

## Voir aussi

- [Documentation officielle de Kotlin sur les chaînes de caractères](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Guide de référence sur les opérateurs en Kotlin](https://kotlinlang.org/docs/reference/operator-overloading.html)
- [Documentation officielle de Kotlin sur la classe StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)