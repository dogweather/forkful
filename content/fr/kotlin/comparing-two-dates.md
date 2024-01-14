---
title:                "Kotlin: Comparaison de deux dates"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
Dans la programmation, il est souvent nécessaire de comparer deux dates. Que ce soit pour déterminer si une date est passée ou future, ou pour trier une liste de dates, la comparaison de dates est un outil essentiel pour tout développeur Kotlin. Dans cet article, nous allons expliquer comment comparer deux dates en utilisant le langage de programmation Kotlin.

## Comment faire
Il existe plusieurs façons de comparer des dates en Kotlin, mais nous allons en explorer deux principales : en utilisant la classe `LocalDate` du package `java.time` et en utilisant des fonctions d'extension.

1. Comparaison avec la classe `LocalDate` :

Pour utiliser la classe `LocalDate`, nous devons l'importer dans notre fichier Kotlin :

```Kotlin
// Importer le package java.time
import java.time.*
```

Ensuite, nous pouvons créer deux instances de la classe `LocalDate` et les comparer à l'aide de la méthode `compareTo()` :

```Kotlin
// Créer deux instances de LocalDate
val date1 = LocalDate.of(2021, 5, 1)
val date2 = LocalDate.of(2022, 7, 15)

// Comparer les deux dates
if (date1.compareTo(date2) < 0) {
    println("$date1 est avant $date2")
}
else if (date1.compareTo(date2) > 0) {
    println("$date1 est après $date2")
}
else {
    println("$date1 et $date2 sont égales")
}

// Output : 2021-05-01 est avant 2022-07-15
```

2. Comparaison avec des fonctions d'extension :

Kotlin permet également d'ajouter des fonctions d'extension à des classes existantes, ce qui est utile pour comparer des dates. Dans cet exemple, nous allons ajouter une fonction d'extension `isBefore()` à la classe `LocalDate` :

```Kotlin
// Ajouter une fonction d'extension pour comparer si une date est avant une autre
fun LocalDate.isBefore(other: LocalDate): Boolean {
    return this.year < other.year ||
            (this.year == other.year && this.monthValue < other.monthValue) ||
            (this.year == other.year && this.monthValue == other.monthValue && this.dayOfMonth < other.dayOfMonth)
}

// Utiliser la fonction d'extension pour comparer les dates
val date1 = LocalDate.of(2021, 5, 1)
val date2 = LocalDate.of(2022, 7, 15)

if (date1.isBefore(date2)) {
    println("$date1 est avant $date2")
}
else if (date1.isAfter(date2)) {
    println("$date1 est après $date2")
}
else {
    println("$date1 et $date2 sont égales")
}

// Output : 2021-05-01 est avant 2022-07-15
```

## Plongée en profondeur
En utilisant la classe `LocalDate` et les fonctions d'extension, il est facile de comparer des dates en Kotlin. Mais il est important de noter que la comparaison est basée sur l'ordre naturel des dates, c'est-à-dire que la date la plus petite sera celle qui se situe avant dans le temps. De plus, la classe `LocalDate` utilise le calendrier grégorien, ce qui signifie que les règles de calendrier telles que les années bissextiles sont prises en compte lors de la comparaison.

## Voir aussi
- [Documentation officielle de la classe `LocalDate`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Documentation officielle sur les fonctions d'extension en Kotlin](https://kotlinlang.org/docs/reference/extensions.html)