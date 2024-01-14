---
title:                "Kotlin: Das aktuelle Datum erhalten."
simple_title:         "Das aktuelle Datum erhalten."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum?
 
Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es kann verwendet werden, um zeitgesteuerte Aktionen auszulösen, Daten zu organisieren oder einfach nur als Teil einer Benutzeroberfläche angezeigt zu werden. Um das aktuelle Datum in einer Kotlin-Anwendung zu erhalten, gibt es verschiedene Ansätze, die wir im Folgenden besprechen werden.

## Wie geht das?
 
Es gibt verschiedene Möglichkeiten, das aktuelle Datum in Kotlin zu erhalten. Eine davon ist die Verwendung der `DateTime`-Klasse aus der `java.time`-Bibliothek. Zunächst müssen wir diese importieren: 

```Kotlin 
import java.time.*
```

Dann können wir einfach eine neue Instanz der `DateTime`-Klasse erstellen und diese in einer Variablen speichern:

```Kotlin 
val currentDate = LocalDateTime.now()
```

Wir können auch das Datum im angegebenen Format ausgeben, indem wir die `format`-Methode verwenden:

```Kotlin
println(currentDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy")))
```

Dies würde das Datum im Format "TT.MM.JJJJ" (z.B. 25.03.2021) ausgeben.

## Deep Dive
 
Wenn wir uns eingehender mit dem Abrufen des aktuellen Datums in Kotlin beschäftigen, können wir verschiedene Optionen entdecken, die je nach Anwendungsfall nützlich sein können. Zum Beispiel können wir auch das aktuelle Datum und die Zeitzone berücksichtigen, indem wir die Methode `now(ZoneId: T)` verwenden:

```Kotlin
val currentDate = ZonedDateTime.now(ZoneId.of("Europe/Berlin"))
```

Dies würde das aktuelle Datum und die aktuelle Uhrzeit in der Zeitzone "Europe/Berlin" zurückgeben.

Eine weitere interessante Option ist die Verwendung der `LocalDate`-Klasse, um nur das aktuelle Datum ohne die Zeitinformationen zu erhalten:

```Kotlin
val currentDate = LocalDate.now()
```

Diese Methode könnte nützlich sein, wenn wir nur das Datum benötigen und nicht an der genauen Uhrzeit interessiert sind.

## Siehe auch
 
- Die offizielle Dokumentation zur Kotlin-Datums- und Uhrzeitverwaltung: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/
- Ein Tutorial zur Arbeit mit dem Date-Time-API in Kotlin: https://www.baeldung.com/kotlin-datetime-api