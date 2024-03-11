---
date: 2024-01-20 17:36:45.683321-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res permet de la formater\
  \ pour l'affichage. Les programmeurs le font pour rendre les dates lisibles par\
  \ les humains\u2026"
lastmod: '2024-03-11T00:14:31.701712-06:00'
model: gpt-4-1106-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res permet de la formater pour\
  \ l'affichage. Les programmeurs le font pour rendre les dates lisibles par les humains\u2026"
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
Convertir une date en chaîne de caractères permet de la formater pour l'affichage. Les programmeurs le font pour rendre les dates lisibles par les humains ou pour les préparer pour le stockage et le traitement.

## How to:
En Kotlin, on utilise la classe `SimpleDateFormat` pour transformer une date en chaîne de caractères. Voici comment faire :

```kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val date = Date()
    val formatter = SimpleDateFormat("dd/MM/yyyy HH:mm:ss")
    val dateString = formatter.format(date)
    println(dateString)
}
```

Sortie exemple :
```
31/03/2023 15:21:47
```

## Deep Dive
Historiquement, Java utilisait `java.util.Date` et `SimpleDateFormat` pour gérer les dates, ce qui a été hérité par Kotlin. Cependant, ces classes avaient des problèmes de thread-safety et de conception. Depuis Java 8, le package `java.time` (JSR-310) offre une meilleure solution avec des classes comme `LocalDate`, `LocalTime`, et `LocalDateTime`.

En Kotlin, on peut aussi utiliser ces classes modernes :

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDateTime = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")
    val formattedDate = currentDateTime.format(formatter)
    println(formattedDate)
}
```

Alternativement, Kotlin a une bibliothèque standard étendue qui inclut des fonctions de formatage de date dans son package `kotlinx.datetime`.

Détails d'implémentation :
- `SimpleDateFormat` n'est pas thread-safe ; ne l'utilisez pas dans un contexte multithread sans précautions.
- `DateTimeFormatter` est immuable et thread-safe, ce qui en fait une meilleure option dans des applications multithread.
- Toujours préciser le format de date souhaité selon les besoins de l'application et la localisation de l'utilisateur.

## See Also
- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/home.html)
- [java.time package overview](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [kotlinx-datetime library](https://github.com/Kotlin/kotlinx-datetime)
