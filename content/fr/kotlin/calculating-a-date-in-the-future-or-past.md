---
title:                "Kotlin: Calculer une date dans le futur ou le passé"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Calculer une date dans le futur ou dans le passé est une tâche courante en programmation. Cela peut être utile pour planifier des événements ou gérer des données en fonction de dates précises. Heureusement, grâce à Kotlin, cette tâche est rapide et simple à mettre en œuvre.

## Comment faire

La première étape pour calculer une date dans le futur ou dans le passé consiste à déterminer la date de référence à partir de laquelle vous souhaitez effectuer le calcul. Par exemple, vous pouvez utiliser la date actuelle en utilisant la fonction `LocalDate.now()`.

Ensuite, pour calculer une date dans le futur, vous devez utiliser la méthode `plusDays()` en lui passant le nombre de jours à ajouter à la date de référence. Par exemple, si vous voulez calculer la date dans 7 jours, vous pouvez écrire:

```Kotlin
val dateDans7Jours = LocalDate.now().plusDays(7)
```

Et si vous voulez calculer une date dans le passé, vous pouvez utiliser la méthode `minusDays()` en lui passant le nombre de jours à soustraire de la date de référence. Par exemple, pour calculer la date d'il y a 3 jours, vous pouvez écrire:

```Kotlin
val dateIlYa3Jours = LocalDate.now().minusDays(3)
```

Vous pouvez également utiliser d'autres méthodes telles que `plusWeeks()`, `plusMonths()`, `plusYears()`, `minusWeeks()`, `minusMonths()` et `minusYears()` pour calculer des dates en fonction de semaines, mois ou années.

## Plongée en profondeur

En utilisant les méthodes mentionnées ci-dessus, vous pouvez calculer une date en utilisant des valeurs entières pour les jours, semaines, mois et années. Mais que se passe-t-il si vous voulez calculer une date en utilisant des valeurs précises comme par exemple, une heure, une minute ou même une seconde?

Pour cela, Kotlin propose la classe `LocalDateTime` qui est une combinaison d'une date et d'un temps précis. Vous pouvez utiliser les méthodes `plus` et `minus` pour ajouter ou soustraire des valeurs précises à une date et obtenir une `LocalDateTime`.

Par exemple, si vous voulez calculer une date avec une heure précise, vous pouvez écrire:

```Kotlin
val dateAvecHeure = LocalDateTime.now().plusHours(5)
```

Et si vous voulez calculer une date avec une minute précise, vous pouvez écrire:

```Kotlin
val dateAvecMinute = LocalDateTime.now().minusMinutes(30)
```

Ainsi, grâce à `LocalDateTime`, vous pouvez effectuer des calculs de dates encore plus précis et personnalisés en fonction de vos besoins.

## Voir aussi

- [Documentation officielle sur Kansas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-kotlin.-long/kotlin.time/index.html)
- [Calcul d'une date dans le futur ou le passé en Java](https://www.baeldung.com/java-date-to-localdate-and-localdatetime)
- [Exemples de manipulation de dates en Kotlin](https://blog.simon-wirtz.de/kotlin-working-with-dates/)