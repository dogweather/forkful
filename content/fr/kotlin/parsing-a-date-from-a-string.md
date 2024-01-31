---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:37:14.106356-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Parser une date, c'est transformer une chaîne de caractères qui représente la date en un objet `Date` que Kotlin comprend et peut utiliser. On fait ça parce qu'en informatique, c'est plus pratique de manipuler des dates dans un format standard quand on veut faire des calculs dessus, comme comparer des dates ou ajouter des temps.

## Comment faire :
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-03-27"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(dateString, formatter)
    
    println(date) // Affiche: 2023-03-27
}
```

## Plongeon Profond
Historiquement, les développeurs Java utilisaient `SimpleDateFormat` pour parser les dates, mais cette classe n'était pas thread-safe et causait des problèmes. Depuis Java 8, et par héritage dans Kotlin, on préfère utiliser `java.time`, plus moderne et sûr. Alternativement, vous pourriez utiliser des bibliothèques de tiers comme Joda-Time ou kotlinx-datetime pour Kotlin multiplateforme. Cela dit, savoir que `DateTimeFormatter` peut lancer une exception `DateTimeParseException` s'il rencontre une chaîne non conforme est crucial. La personnalisation est reine : vous pouvez définir précisément le format de votre date avec des patterns de formatage.

## Voir Aussi
2. Guide d'utilisation de `java.time` : [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. Pour aller plus loin avec Joda-Time : [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
4. kotlinx-datetime pour Kotlin multiplateforme : [https://github.com/Kotlin/kotlinx-datetime](https://github.com/Kotlin/kotlinx-datetime)
