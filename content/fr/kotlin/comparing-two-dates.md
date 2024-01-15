---
title:                "Comparaison de deux dates"
html_title:           "Kotlin: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous êtes toujours demandé comment comparer deux dates en Kotlin ? Peut-être que vous avez besoin de vérifier si une date précède une autre pour valider un formulaire ou planifier une tâche dans votre application. Dans cet article, nous allons explorer différentes façons de comparer deux dates en utilisant les fonctionnalités offertes par Kotlin.

## Comment Faire

Pour commencer, nous allons définir deux variables de type `LocalDate` représentant nos deux dates à comparer :

```Kotlin
val date1 = LocalDate.of(2021, 8, 15)
val date2 = LocalDate.of(2021, 8, 20)
```

### Comparer les dates en utilisant les opérateurs de comparaison

Kotlin offre des opérateurs de comparaison pour les types de données `LocalDate` qui facilitent la comparaison. Par exemple, pour vérifier si `date1` est avant `date2`, nous pouvons simplement utiliser l'opérateur `<` :

```Kotlin
println(date1 < date2) // Output: true
```

De la même manière, nous pouvons utiliser les autres opérateurs de comparaison tels que `<=`, `>`, `>=` pour comparer les dates.

### Comparer les dates en utilisant la méthode `compareTo()`

Nous pouvons également utiliser la méthode `compareTo()` pour comparer deux dates en Kotlin. Cette méthode renvoie un entier négatif si la date actuelle est avant la date passée en paramètre, un entier positif si elle est après, et 0 si elles sont égales. Par exemple :

```Kotlin
println(date1.compareTo(date2)) // Output: -5
```

Dans cet exemple, la valeur retournée est -5 car `date1` est 5 jours avant `date2`.

### Vérifier si deux dates sont égales

Pour vérifier si deux dates sont égales, nous pouvons utiliser l'opérateur `==`, qui renvoie `true` si les dates sont égales et `false` sinon.

```Kotlin
val date1 = LocalDate.of(2021, 8, 15)
val date2 = LocalDate.of(2021, 8, 15)

println(date1 == date2) // Output: true
```

## Plongée Profonde

Kotlin offre également la possibilité de comparer des dates avec une précision différente, par exemple en comparant uniquement l'année ou le mois. Pour ce faire, nous pouvons utiliser les méthodes `isBefore()`, `isAfter()` et `equals()` en passant en paramètre `ChronoUnit.YEAR` ou `ChronoUnit.MONTH`.

```Kotlin
val date1 = LocalDate.of(2021, 8, 15)
val date2 = LocalDate.of(2021, 8, 20)

println(date2.isAfter(date1, ChronoUnit.MONTHS)) // Output: true
```

De plus, Kotlin offre également la possibilité de récupérer la différence entre deux dates en utilisant la méthode `until()` qui renvoie un objet `Period`. Par exemple :

```Kotlin
val date1 = LocalDate.of(2021, 8, 15)
val date2 = LocalDate.of(2021, 8, 20)

val difference = date1.until(date2)
println(difference.days) // Output: 5
```

## Voir Aussi

- [Documentation officielle Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Guide de comparaison des dates en Java](https://www.baeldung.com/java-date-compare)