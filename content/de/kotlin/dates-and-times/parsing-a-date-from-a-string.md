---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:28.358626-07:00
description: "Das Parsen eines Datums aus einem String beinhaltet die Umwandlung von\
  \ Text in ein Date-Objekt. Diese Operation ist grundlegend f\xFCr Anwendungen, die\
  \ mit\u2026"
lastmod: '2024-03-13T22:44:53.855655-06:00'
model: gpt-4-0125-preview
summary: Das Parsen eines Datums aus einem String beinhaltet die Umwandlung von Text
  in ein Date-Objekt.
title: Einen Datum aus einem String analysieren
weight: 30
---

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet die Umwandlung von Text in ein Date-Objekt. Diese Operation ist grundlegend für Anwendungen, die mit von Nutzern eingegebenen oder aus externen Datensätzen bezogenen Daten interagieren, da sie eine einfache Manipulation und Formatierung nach Bedarf ermöglicht.

## Wie geht das:
Kotlin unterstützt das Parsen von Daten durch das `java.time`-Paket, das in Java 8 eingeführt wurde. Hier ist ein einfacher Ansatz unter Verwendung von `LocalDateTime` und einem spezifischen Muster:

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
    println(date)  // Ausgabe: 2023-04-01T12:00
}
```

Für mehr Flexibilität oder um Daten aus externen Quellen wie APIs zu verarbeiten, könntest du eine Drittanbieterbibliothek wie Joda-Time verwenden (obwohl dies mit dem robusten `java.time` weniger häufig vorkommt). Allerdings wird für die meisten Kotlin-Anwendungen der moderne Ansatz, der von der JDK bereitgestellt wird, bevorzugt.

Um ein Datum in Kotlin ohne die Verwendung von Drittanbieterbibliotheken zu parsen, kannst du auch die Klasse `SimpleDateFormat` für Versionen vor Java 8 oder Android-API-Ebenen nutzen, die `java.time` nicht unterstützen:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // Die Ausgabe variiert je nach Zeitzone, z.B., Sat Apr 01 12:00:00 GMT 2023
}
```

Denke immer daran, die Zeitzone zu setzen, wenn du mit `SimpleDateFormat` arbeitest, um unerwartete Verschiebungen in den geparsten Daten zu vermeiden.
