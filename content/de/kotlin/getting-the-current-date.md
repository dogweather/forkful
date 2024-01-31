---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:24.922008-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Abrufen des aktuellen Datums ist eine gängige Funktion – es erlaubt, genau zu wissen, welches Datum heute ist. Programmierer nutzen es für Features wie Kalender, Protokolle oder Gültigkeitsprüfungen.

## How to: (Wie geht das?)
Mit Kotlin kannst du das aktuelle Datum auslesen und es in deinem Code verwenden:

```Kotlin
import java.time.LocalDate

fun main() {
    val heute = LocalDate.now()
    println(heute)
}

// Beispiel Ausgabe:
// 2023-04-15
```

Formatierung anpassen? Kein Problem:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    val heute = LocalDate.now().format(dateFormat)
    println(heute)
}

// Beispiel Ausgabe:
// 15.04.2023
```

## Deep Dive (Tiefergehende Infos)
Das Konzept der Zeit in Programmen hat eine komplexe Geschichte. Früher war das Abrufen des Datums und der Uhrzeit oft plattformabhängig. Java löste einige dieser Probleme mit dem `java.util.Date`, aber das war noch nicht perfekt. `java.time.LocalDate` kam mit Java 8 und gab uns eine bessere, unveränderliche (immutable) Datumsklasse. 

Alternativen? Für ältere Java-Versionen oder Android SDKs kann `Calendar.getInstance()` gebraucht werden. Drittanbieter-Bibliotheken wie Joda-Time boten Lösungen, bevor `java.time` zum Standard wurde.

Implementierungsdetails? `LocalDate.now()` nutzt die Systemuhr im Standard-Zeitzone deines Geräts. Es handelt sich um ein unveränderliches Objekt (immutable object), was in der Welt der Multi-Thread-Anwendungen sicherheits- und nutzungsvorteilhaft ist.

## See Also (Siehe auch)
- [Oracle's Java documentation for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Baeldung's Guide to LocalDate](https://www.baeldung.com/java-8-date-time-intro)
- [Joda-Time Library](http://www.joda.org/joda-time/) (falls `java.time` nicht verfügbar ist)
