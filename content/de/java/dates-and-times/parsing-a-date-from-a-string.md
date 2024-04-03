---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:14.736822-07:00
description: "Das Parsen eines Datums aus einem String beinhaltet die Umwandlung der\
  \ Textdarstellung eines Datums und einer Zeit in ein `Date`-Objekt oder ein\u2026"
lastmod: '2024-03-13T22:44:53.771922-06:00'
model: gpt-4-0125-preview
summary: Das Parsen eines Datums aus einem String beinhaltet die Umwandlung der Textdarstellung
  eines Datums und einer Zeit in ein `Date`-Objekt oder ein moderneres `LocalDateTime`-Objekt.
title: Einen Datum aus einem String analysieren
weight: 30
---

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet die Umwandlung der Textdarstellung eines Datums und einer Zeit in ein `Date`-Objekt oder ein moderneres `LocalDateTime`-Objekt. Programmierer tun dies, um Daten zu manipulieren, zu formatieren, zu vergleichen oder in einem standardisierten Format zu speichern, was für Anwendungen, die Datumsberechnungen, Validierungen oder eine konsistente Internationalisierung erfordern, entscheidend ist.

## Wie geht das:

### Verwendung des `java.time` Pakets (Empfohlen in Java 8 und später):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Ausgabe: 2023-04-30
    }
}
```

### Verwendung von `SimpleDateFormat` (Älterer Ansatz):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // Ausgabeformat hängt von Ihrem Systemstandardformat ab
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Verwendung von Drittanbieter-Bibliotheken (z.B. Joda-Time):
Joda-Time war eine bedeutende Drittanbieter-Bibliothek, befindet sich jetzt jedoch aufgrund der Einführung des `java.time` Pakets in Java 8 im Wartungsmodus. Für diejenigen, die Java-Versionen vor 8 verwenden, ist Joda-Time jedoch eine gute Wahl.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Ausgabe: 2023-04-30
    }
}
```
Beachten Sie, dass Sie immer auf die Zeitzoneneinstellungen achten sollten, wenn Sie Datumszeiten parsen oder formatieren, statt nur Datumsangaben.
