---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:36:48.684984-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String bedeutet, dass ein Text, der ein Datum repräsentiert, in ein Date-Objekt umgewandelt wird. Programmierer müssen das oft tun, um mit den Daten weiterarbeiten zu können – sei es zum Speichern, Vergleichen oder für Berechnungen.

## So geht's:

Java bietet die `java.time`-Bibliothek, die das Parsen von Datumsstrings vereinfacht. Hier ist ein schnelles Beispiel:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DatumParsen {
    public static void main(String[] args) {
        String datumString = "2023-04-01";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate datum = LocalDate.parse(datumString, formatter);
        System.out.println(datum); // Gibt das geparse Datum aus
    }
}
```

Ausgabe:

```
2023-04-01
```

## Tiefere Einblicke:

Früher haben Java-Entwickler `SimpleDateFormat` aus `java.text` verwendet, das nicht thread-safe war und ein weniger intuitives API hatte. Mit Java 8 kam `java.time`, bekannt als JSR-310, einem viel sichereren und flexibleren System, das durch Joda-Time inspiriert wurde.

Alternativen zum `DateTimeFormatter` sind Bibliotheken von Drittanbietern wie Joda-Time, die vor Java 8 oft benutzt wurden. Auch `java.util.Date` und `java.util.Calendar` sind ältere Alternativen, die aber als veraltet gelten.

Bei der Implementierung ist zu beachten, dass nicht alle Datumsstrings gleich sind. Einige haben Zeitstempel, andere haben keine, und das Format variiert. Der `DateTimeFormatter` muss passend zum zu parsenden String erstellt werden.

## Siehe auch:

- [Java 8 Date/Time guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [DateTimeFormatter JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)