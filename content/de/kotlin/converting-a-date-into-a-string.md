---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Manchmal müssen wir Daten (Kalenderdaten) in Strings (Textketten) umwandeln. Warum? Grund ist, dass wir sie anpassen und formatieren können, um auf unterschiedliche Weise dargestellt zu werden.

## So geht's:

Um in Kotlin ein Datum in einen String zu konvertieren, verwenden wir die `java.time.format.DateTimeFormatter` Klasse:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val datum = LocalDate.now()
    val format = DateTimeFormatter.ofPattern("dd.MM.yyyy")

    val datumAlsString = datum.format(format)

    println(datumAlsString)  // Ausgabe: wird das heutige Datum im Format "dd.MM.yyyy" ansehen
}
```

## Vertiefung:

Historisch gesehen stammt unsere aktuelle Methode zum Datum-zu-String-Konvertierung von Javas `SimpleDateFormat` Klasse. 
Alternativen könnten die Verwendung von Bibliotheken wie Joda-Time sein. Allerdings bringt die direkte Verwendung von Java 8's `java.time` Paket viele Vorteile, darunter bessere Leistung und größere Einfachheit.

## Siehe auch:

- Offizielle Kotlin Dokumentation: https://kotlinlang.org/docs/home.html
- Java DateTimeFormatter Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Zeit- und Datumsbibliotheken Vergleich: https://www.baeldung.com/java-8-date-time-intro