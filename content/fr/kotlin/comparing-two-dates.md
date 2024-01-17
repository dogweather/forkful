---
title:                "Comparer deux dates"
html_title:           "Kotlin: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Comparer deux dates est essentiel pour les programmeurs car cela leur permet de déterminer l'ordre chronologique des événements dans leurs programmes. Cela peut être utile pour des fonctionnalités telles que la planification, la mise à jour des données en fonction des dates et bien d'autres.

## Comment faire:
```Kotlin
// Comparer deux dates en utilisant les opérateurs >, < et =
var date1 = Date(2021, 7, 15)
var date2 = Date(2021, 7, 20)
if (date1 > date2) {
  println("La date 1 est après la date 2")
} else if (date1 < date2) {
  println("La date 1 est avant la date 2")
} else {
  println("Les deux dates sont égales")
}

// Comparer deux LocalDate en utilisant isAfter(), isBefore() et isEqual()
var date1 = LocalDate.of(2021, 7, 15)
var date2 = LocalDate.of(2021, 7, 20)
if (date1.isAfter(date2)) {
  println("La date 1 est après la date 2")
} else if (date1.isBefore(date2)) {
  println("La date 1 est avant la date 2")
} else {
  println("Les deux dates sont égales")
}
```

## Plongez plus en profondeur:
Comparé à d'autres langages de programmation, Kotlin offre une syntaxe simple et concise pour comparer deux dates. D'autres alternatives incluent l'utilisation de la librairie Java.util.date ou la création de fonctions personnalisées pour comparer les dates selon différents critères, tels que l'année, le mois et le jour.

Lors de la comparaison de deux dates, il est important de prendre en compte les fuseaux horaires et les notions de jours fériés qui peuvent affecter le résultat. La classe LocalDate de Kotlin est basée sur le calendrier grégorien, ce qui peut être un avantage ou un inconvénient selon le cas d'utilisation.

## Voir aussi:
- [Documentation officielle de Kotlin pour la classe Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Documentation officielle de Kotlin pour la classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Différences entre les dates en Kotlin et Java](https://blog.mindorks.com/differences-between-dates-in-kotlin-and-java)