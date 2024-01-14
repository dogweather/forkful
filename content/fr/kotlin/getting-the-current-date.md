---
title:                "Kotlin: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous utilisons souvent la date dans nos programmes pour diverses raisons telles que la gestion des tâches, la planification des événements ou simplement pour afficher la date actuelle dans une application. Kotlin offre une manière simple et efficace pour obtenir la date actuelle dans vos programmes.

## Comment obtenir la date actuelle en utilisant Kotlin

Pour obtenir la date actuelle en utilisant Kotlin, on peut utiliser la classe `LocalDate` qui fait partie du package `java.time`. Cela signifie qu'il faut importer le package pour pouvoir utiliser cette classe. Ensuite, on peut utiliser la méthode `now()` pour obtenir une instance de `LocalDate` correspondant à la date et l'heure actuelles. Voici un exemple de code:

```Kotlin
import java.time.LocalDate

fun main(args: Array<String>) {
    val date = LocalDate.now()
    println("La date actuelle est: $date")
}
```

La sortie de ce code serait quelque chose comme "La date actuelle est: 2021-05-24". Cela dépendra de la date à laquelle vous exécutez le code.

## Plongée en profondeur

La classe `LocalDate` offre de nombreuses méthodes utiles pour travailler avec des dates, telles que `plusDays()`, `plusMonths()`, etc. Vous pouvez également formater la date en utilisant la méthode `format()` et le format spécifié. Par exemple, pour formater la date au format "dd/MM/yyyy", vous pouvez utiliser la ligne de code suivante:

```Kotlin
val formattedDate = date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
```

Il est également possible de créer une instance de `LocalDate` à partir d'une date spécifique en utilisant la méthode `of()`. Par exemple, pour créer une instance de `LocalDate` pour le 1er janvier 2022, vous pouvez utiliser la ligne de code suivante:

```Kotlin
val newYear = LocalDate.of(2022, 1, 1)
```

N'hésitez pas à explorer les différentes méthodes disponibles dans la classe `LocalDate` pour manipuler et formater les dates selon vos besoins.

## Voir aussi

- [Documentation officielle de Kotlin sur la classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Tutoriel sur les dates en Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)
- [Documentation officielle de Java sur la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)