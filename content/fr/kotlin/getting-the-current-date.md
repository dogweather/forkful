---
title:                "Obtenir la date actuelle"
html_title:           "Kotlin: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la récupération de la date courante, et pourquoi les programmeurs le font-ils?

La récupération de la date courante est une fonctionnalité essentielle en programmation, qui permet aux programmeurs d'obtenir la date et l'heure actuelles. Cela peut être utile pour enregistrer le moment exact où un événement s'est produit, ou pour gérer des tâches planifiées en fonction de la date et de l'heure actuelles.

## Comment faire?

Voici deux façons de récupérer la date courante en Kotlin:

- En utilisant la classe `LocalDateTime`: ```Kotlin
val currentDateTime = LocalDateTime.now()
println(currentDateTime)
// Output: 2021-01-01T12:30:00.000
```
- En utilisant la classe `Date`: ```Kotlin
val currentDate = Date()
println(currentDate)
// Output: Fri Jan 01 12:30:00 GMT 2021 
```

## Plongée en profondeur

La récupération de la date courante est une fonctionnalité qui existe depuis les premiers jours de la programmation informatique. Cependant, avec les avancées technologiques, sa mise en œuvre a évolué pour être plus précise et plus fiable. Il existe plusieurs alternatives pour obtenir la date courante en Kotlin, notamment en utilisant des librairies tierces telles que JodaTime ou ThreeTen.

En ce qui concerne l'implémentation en Kotlin, la classe `LocalDateTime` utilise la bibliothèque Java `java.time` pour gérer les dates et les heures, tandis que la classe `Date` est dépréciée depuis Java 8 et recommandée pour les versions antérieures.

## Voir aussi

- [Documentation officielle de Kotlin sur les dates et les heures](https://kotlinlang.org/docs/datetime/)
- [Documentation officielle de Java sur `java.time`](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Librairie JodaTime pour la gestion des dates et des heures en Java](https://www.joda.org/joda-time/)
- [Librairie ThreeTen pour la gestion des dates et des heures en Java 8 et versions ultérieures](https://www.threeten.org/)