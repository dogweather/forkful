---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates signifie déterminer laquelle est antérieure ou postérieure. Les programmeurs le font souvent pour trier des événements dans l'ordre chronologique ou pour calculer la durée entre deux dates.

## Comment:

Voici un exemple de comparaison de deux dates en Kotlin.

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 12, 15)
    val date2 = LocalDate.of(2021, 1, 1)

    println(if (date1.isBefore(date2)) "date1 est avant date2" 
             else "date1 n'est pas avant date2")
}
```

Cet exemple générera la sortie suivante: 

```Kotlin
date1 est avant date2
```

## Pour aller plus loin

L'API `LocalDate` de Java (introduite dans Java 8) a optimisé la manipulation de dates en évitant les aspects complexes des anciennes API de date comme `java.util.Date` et `java.util.Calendar`. En Kotlin, nous utilisons principalement `LocalDate` pour comparer les dates.

Alternativement, pour les cas où vous devez gérer des heures précises, utilisez `LocalDateTime` ou `ZonedDateTime`. 

Lors de la comparaison de dates, gardez à l'esprit les détails d'implémentation, tels que les zones horaires et les secondes fractionnaires. Surtout, n'oubliez pas que l'année n'est pas toujours de 365 jours à cause des années bissextiles !

## Voir Aussi

Pour plus d'informations sur l'utilisation de `LocalDate` en Kotlin, consultez ce lien:

https://www.baeldung.com/kotlin/dates

Vous pouvez également trouver des informations utiles sur l'API `java.time` ici:

https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html.