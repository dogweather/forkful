---
date: 2024-01-20 17:36:51.848668-07:00
description: "Datum in String umzuwandeln bedeutet, ein Datumsobjekt in einen Text\
  \ umzuformen, der menschenlesbar ist. Programmierer machen das, weil Daten oft in\
  \ einer\u2026"
lastmod: '2024-03-13T22:44:53.773938-06:00'
model: gpt-4-1106-preview
summary: Datum in String umzuwandeln bedeutet, ein Datumsobjekt in einen Text umzuformen,
  der menschenlesbar ist.
title: Datum in einen String umwandeln
weight: 28
---

## Was & Warum?
Datum in String umzuwandeln bedeutet, ein Datumsobjekt in einen Text umzuformen, der menschenlesbar ist. Programmierer machen das, weil Daten oft in einer für Menschen verständlichen Form angezeigt oder gespeichert werden müssen.

## So geht's:
```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DatumInString {
    public static void main(String[] args) {
        // Aktuelle Zeit erzeugen
        LocalDateTime jetzt = LocalDateTime.now();
        
        // Formatter definieren
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm");
        
        // Datum in String umwandeln
        String datumAlsString = jetzt.format(formatter);
        
        // Ergebnis ausgeben
        System.out.println(datumAlsString);
    }
}
```

Beispiel-Ausgabe:
```
31.03.2023 15:42
```

## Deep Dive
Früher mussten Java-Entwickler `SimpleDateFormat` aus `java.text` nutzen, das war umständlich und nicht thread-sicher. Seit Java 8 gibt es die `java.time`-API, die das Verarbeiten von Datums- und Zeitangaben erleichtert. Es gibt Alternativen wie `Date`, `Calendar` und externe Bibliotheken wie Joda-Time, die aber heutzutage weniger benutzt werden. Intern wird beim Umwandeln ein Formatierungs-String benutzt, der Regeln wie "dd" für Tage und "MM" für Monate vorgibt. Das Format ist flexibel und kann angepasst werden, um verschiedene Output-Stile zu erzielen.

## Weitere Informationen
- [Oracle JavaDocs für DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung Guide zu Java 8 Date and Time](https://www.baeldung.com/java-8-date-time-intro)
- [Java Date and Time API Tutorial](https://www.tutorialspoint.com/java8/java8_datetime_api.htm)
