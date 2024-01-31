---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:03.644516-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, einen Text in ein Datum umzuwandeln. Programmierer machen das, um datumsbezogene Operationen durchzuführen oder Datumsformate anzupassen.

## How to:
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-03-15"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)

    println(parsedDate) // Gibt 2023-03-15 aus
}
```

## Deep Dive
Das Parsen von Datumsangaben aus Strings ist eine Standardoperation, die seit den Anfängen der Programmierung existiert. Vor Java 8 verwendeten Programmierer `SimpleDateFormat`, das weniger sicher und weniger intuitiv war. Ab Java 8 und somit auch in Kotlin bietet `DateTimeFormatter` eine robuste Alternative.

Alternativen zum Standard `DateTimeFormatter` umfassen Bibliotheken wie Joda-Time, die vor Java 8 häufig verwendet wurden, aber seither oft als veraltet gelten.

Beim Implementation sollten Zeitzonen (`ZoneId`) und Lokalisierungsaspekte (`Locale`) beachtet werden, falls diese relevant sind.

## See Also
- [Oracle JavaDocs zu DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
