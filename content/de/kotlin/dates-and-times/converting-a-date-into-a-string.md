---
date: 2024-01-20 17:36:59.435363-07:00
description: "Das Umwandeln eines Datums in einen String bedeutet, ein Datum so zu\
  \ formatieren, dass es als lesbare Zeichenkette dargestellt wird. Programmierer\
  \ machen\u2026"
lastmod: '2024-03-13T22:44:53.857645-06:00'
model: gpt-4-1106-preview
summary: "Das Umwandeln eines Datums in einen String bedeutet, ein Datum so zu formatieren,\
  \ dass es als lesbare Zeichenkette dargestellt wird. Programmierer machen\u2026"
title: Datum in einen String umwandeln
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Datums in einen String bedeutet, ein Datum so zu formatieren, dass es als lesbare Zeichenkette dargestellt wird. Programmierer machen das, um Daten benutzerfreundlich in Apps und Webseiten anzuzeigen oder sie in einem bestimmten Format zu speichern.

## So geht's:
```Kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val currentDate = Date()
    val dateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
    val dateString = dateFormat.format(currentDate)
    
    println(dateString) // Beispiel Ausgabe: 31.03.2023 17:45:12
}
```

## Ausführlicher Einblick
Ursprünglich verwendete Kotlin für Datumsoperationen die Klassen aus `java.util` wie `Date` und `SimpleDateFormat`. Obwohl immer noch verwendbar, empfiehlt sich inzwischen die Nutzung von `java.time`, der modernen Java Date and Time API, die seit Java 8 verfügbar ist und durch desugaring auch auf älteren Android-Geräten verwendet werden kann.

Alternativen zur `SimpleDateFormat` könnten in der Nutzung der `java.time` Klassen wie `LocalDateTime` und `DateTimeFormatter` liegen. Diese bieten mehrere Vorteile, einschließlich Unveränderlichkeit (thread-sicher) und eine umfangreichere API für Datums- und Zeitmanipulationen.

Eine typische Implementierung mit `java.time` könnte so aussehen:
```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDateTime = LocalDateTime.now()
    val dateTimeFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss")
    val dateTimeString = currentDateTime.format(dateTimeFormat)
    
    println(dateTimeString) // Beispiel Ausgabe: 31.03.2023 17:45:12
}
```
Dabei können unterschiedliche Pattern für `DateTimeFormatter` verwendet werden, je nachdem, in welchem Format das Datum als String repräsentiert werden soll.

## Siehe auch
- [Oracle Java Dokumentation zu DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Android Desugaring der Java Time API](https://developer.android.com/studio/write/java8-support#library-desugaring)
