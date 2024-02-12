---
title:                "Analyser une date depuis une chaîne de caractères"
aliases: - /fr/kotlin/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:36.937646-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse d'une date à partir d'une chaîne de caractères implique de convertir du texte en un objet Date. Cette opération est fondamentale pour les applications qui interagissent avec des dates saisies par les utilisateurs ou provenant de jeux de données externes, permettant ainsi une manipulation et un formatage facile selon les besoins.

## Comment faire :
Kotlin prend en charge l'analyse de dates via le package `java.time`, introduit dans Java 8. Voici une approche simple utilisant `LocalDateTime` et un motif spécifique :

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Sortie : 2023-04-01T12:00
}
```

Pour plus de flexibilité, ou pour gérer des dates provenant de sources externes comme les API, vous pourriez utiliser une bibliothèque tierce telle que Joda-Time (bien que cela soit moins courant maintenant avec `java.time` étant robuste). Cependant, il est préférable de s'en tenir à l'approche moderne fournie par le JDK pour la plupart des applications Kotlin.

Pour analyser une date en Kotlin sans utiliser de bibliothèques tierces, vous pouvez également utiliser la classe `SimpleDateFormat` pour les versions antérieures à Java 8 ou les niveaux d'API Android qui ne prennent pas en charge `java.time` :

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // La sortie variera en fonction de votre fuseau horaire, par exemple, Sam Apr 01 12:00:00 GMT 2023
}
```

N'oubliez pas de toujours définir le fuseau horaire si vous travaillez avec `SimpleDateFormat` pour éviter des décalages inattendus dans les dates analysées.
