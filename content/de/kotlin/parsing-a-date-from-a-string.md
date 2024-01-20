---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String bedeutet, einen Text mit Datum und Uhrzeit in eine Datenstruktur umzuwandeln, die im Code verwendet werden kann. Dies ist nützlich, um Daten aus externen Quellen zu lesen oder ein menschenlesbares Format in etwas umzuwandeln, das für Berechnungen verwendet werden kann.

## So geht's:

In Kotlin ist das Parsen eines Datums aus einem String ziemlich einfach. Hier ist ein einfaches Beispiel:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2022-10-20"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)
    println(parsedDate)
}
```

Wenn Sie diesen Code ausführen, sehen Sie folgenden Output:

```Kotlin
2022-10-20
```

## Tiefer eintauchen:

Historisch gesehen musste man früher komplexe Algorithmen implementieren, um ein Datum aus einem String zu parsen. Heutzutage ermöglichen uns Bibiliotheken wie `java.time.format.DateTimeFormatter` in Kotlin, dies auf einfache Weise zu erreichen.

Alternativen zur `DateTimeFormatter`-Klasse umfassen die `SimpleDateFormat`-Klasse, die ähnlich funktioniert, aber eine andere Syntax verwendet.

Wenn es um Implementierungsdetails geht, entspricht das Muster, das Sie in `ofPattern` zur Verfügung stellen, genau dem Format des Datums in Ihrem String. Wenn diese nicht übereinstimmen, schlägt das Parsen fehl und Sie erhalten eine Ausnahme.

## Siehe auch:

- [Umgang mit Daten und Zeiten in Kotlin - baeldung.com](https://www.baeldung.com/kotlin/dates)
- [SimpleDateFormat - Oracle Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)