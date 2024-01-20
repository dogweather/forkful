---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was und Warum?

Mit Kotlin das aktuelle Datum abzurufen bedeutet, in der Zeit zu 'leben'. Es ist wichtig für Anwendungen, die Zeitstempel benötigen, für Ereignisprotokollierung oder Datums- und Zeitvalidierung.

## So geht's:

Mit der Standardbibliothek von Kotlin ist das Abrufen des aktuellen Datums einfach. Hier ist ein einfacher Weg:

```kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("Aktuelles Datum: $currentDate")
}
```
Wenn Sie das ausführen, wird so etwas wie folgt ausgegeben:

```bash
Aktuelles Datum: 2022-01-01
```
Sie können auch eine Instanz von `java.util.Date` verwenden:

```kotlin
import java.util.Date

fun main() {
    val date = Date()
    println("Aktuelles Datum und Uhrzeit: $date")
}
```
Das gibt das aktuelle Datum und die Uhrzeit im Standardformat aus.

## Tiefere Einsicht

Historisch gesehen hat JDK mehrere Methoden für das Arbeiten mit Datum und Uhrzeit zur Verfügung gestellt, einschließlich `java.util.Date` und `java.util.Calendar`. In den letzten Jahren hat jedoch das moderne `java.time` Paket weite Anerkennung gefunden aufgrund seiner Verbesserungen bezüglich Sicherheit, Leistung und intuitive API-Design.

Als eine alternative Methode können Sie `java.util.Calendar` verwenden, um das aktuelle Datum zu bekommen:

```kotlin
import java.util.Calendar

fun main() {
    val calendar = Calendar.getInstance()
    println("Aktuelles Datum: ${calendar.time}")
}
```

In Bezug auf die Implementierung zu beachten ist, dass `java.time.LocalDate.now` und `java.util.Date` die Systemzeitzone verwenden, um das aktuelle Datum abzurufen, während `java.util.Calendar.getInstance` die Standardzeitzone verwendet.

## Siehe auch

Für mehr Details und weiterführende Informationen, sehen Sie bitte die offizielle Dokumentation:

- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)