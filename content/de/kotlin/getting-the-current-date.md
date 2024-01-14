---
title:    "Kotlin: Das aktuelle Datum erhalten"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum zu erhalten, ist eine entscheidende Aufgabe in der Programmierung. Es ermöglicht uns, dynamische und relevante Informationen in unseren Anwendungen darzustellen. Egal ob wir einen Terminkalender erstellen oder die Daten in einer Datenbank organisieren, das aktuelle Datum ist ein wichtiger Teil unserer Programmlogik.

## Wie man das aktuelle Datum in Kotlin erhält

Um das aktuelle Datum in Kotlin zu erhalten, können wir die `LocalDate`-Klasse aus der Java-Kotlin-Interop-Bibliothek verwenden. Diese Klasse stellt Methoden für die Date-Berechnung und -Manipulation zur Verfügung.

```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()
println(currentDate)
```
Output: 2020-07-14

Wir können auch bestimmte Formate für das Datum festlegen, indem wir die `format`-Methode verwenden.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val currentDate = LocalDate.now()
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))
println(formattedDate)
```
Output: 14.07.2020

## Tiefer gehende Informationen

Die `LocalDate`-Klasse basiert auf dem gregorianischen Kalendersystem und unterstützt Datumsberechnungen bis zum Jahr 999 999 999. Sie enthält auch Methoden für die Berechnung von Zeitzonen und Schaltjahre.

Eine Alternative zur `LocalDate`-Klasse ist die `Calendar`-Klasse, die ebenfalls in der Java-Kotlin-Interop-Bibliothek enthalten ist und eine erweiterte Funktionalität für die Datums- und Zeitberechnung bietet.

## Siehe auch

- [Java-Kotlin-Interop-Bibliothek](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-java-time/index.html)
- [Offizielle Kotlin-Dokumentation zu Datums- und Zeitfunktionen](https://kotlinlang.org/docs/reference/datetime.html)
- [Java-Dokumentation zu Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)