---
title:                "Kotlin: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est une tâche courante en programmation, que ce soit pour vérifier si une date est égale, antérieure ou postérieure à une autre. Cela peut être utile pour des fonctionnalités telles que la vérification de la validité d'une carte d'identité ou la gestion des dates d'expiration.

## Comment faire

Pour comparer deux dates en Kotlin, vous pouvez utiliser la méthode `compareTo()` de la classe `LocalDate`. Voici un exemple de code qui compare deux dates et affiche le résultat :

```Kotlin
fun main() {
    val date1 = LocalDate.of(2021, 5, 15)
    val date2 = LocalDate.of(2021, 5, 20)
    val comparison = date1.compareTo(date2)
    if (comparison < 0) {
        println("La date 1 est antérieure à la date 2.")
    } else if (comparison > 0) {
        println("La date 1 est postérieure à la date 2.")
    } else {
        println("Les deux dates sont égales.")
    }
}
```
Résultat :
```
La date 1 est antérieure à la date 2.
```

Vous pouvez également utiliser les méthodes `isBefore()` et `isAfter()` pour vérifier si une date est antérieure ou postérieure à une autre. Voici un exemple de code qui utilise ces méthodes :

```Kotlin
fun main() {
    val date1 = LocalDate.of(2021, 5, 15)
    val date2 = LocalDate.of(2021, 5, 20)
    if (date1.isBefore(date2)) {
        println("La date 1 est antérieure à la date 2.")
    } else if (date1.isAfter(date2)) {
        println("La date 1 est postérieure à la date 2.")
    } else {
        println("Les deux dates sont égales.")
    }
}
```

Résultat :
```
La date 1 est antérieure à la date 2.
```

## Plongeons en profondeur

Il est important de noter que la comparaison des dates en Kotlin se fait sur la base du calendrier ISO, qui suit le système grégorien et ne prend pas en compte les éventuels changements de calendrier dans l'histoire. Il est donc important de toujours vérifier si le calendrier utilisé convient à votre utilisation avant de comparer des dates.

Un autre aspect à prendre en compte lors de la comparaison des dates est la prise en compte ou non de l'heure. En utilisant la classe `LocalDateTime`, qui représente une date et une heure sans fuseau horaire, vous pouvez comparer des dates en tenant compte de l'heure. Voici un exemple de code :

```Kotlin
fun main() {
    val date1 = LocalDateTime.of(2021, 5, 15, 10, 30, 0)
    val date2 = LocalDateTime.of(2021, 5, 15, 9, 0, 0)
    val comparison = date1.compareTo(date2)
    if (comparison < 0) {
        println("La date 1 est antérieure à la date 2.")
    } else if (comparison > 0) {
        println("La date 1 est postérieure à la date 2.")
    } else {
        println("Les deux dates sont égales.")
    }
}
```

Résultat :
```
La date 1 est postérieure à la date 2.
```

## Voir aussi

- [Documentation officielle de Kotlin sur les dates et les heures](https://kotlinlang.org/docs/datetime.html)
- [Cours de programmation en Kotlin en français](https://openclassrooms.com/fr/courses/4564371-programmez-avec-le-language-kotlin)