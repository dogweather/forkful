---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

---

## Was & Warum?
Die Konvertierung eines Datums in einen String in Java ist ein durchaus üblicher Vorgang, bei dem ein Datum in seinen textbasierten Repräsentation umgewandelt wird. Dies erleichtert die Manipulation, Anzeige oder Speicherung von Datumsinformationen und supports Kompatibilität mit nicht-nativen Datentypen.

## Wie geht's:
Hier ist ein Beispiel, wie man das Java 8 `DateTimeFormatter` verwendet, um ein `LocalDate` in einen String zu konvertieren:

```Java 
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Program {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        String dateString = date.format(formatter);
        System.out.println(dateString);
    }
}
```

Laufen Sie das, und Sie werden so etwas wie `10.07.2022` auf Ihrer Konsole sehen.

## Tiefgehende Information
Java unterstützte zunächst Datumsformatierung mit `SimpleDateFormat`. Allerdings stellte dies einige Schwierigkeiten dar, wie Thread-Unsicherheit und umständliches API-Design. Mit der Einführung von Java 8 wurde eine neuere, verbesserte API eingeführt: die `DateTimeFormatter` Klasse. Es ist thread-sicher und bietet flexiblere Formatierungsoptionen.

Alternativ zu `DateTimeFormatter`, könnten Sie externe Bibliotheken wie `Joda-Time` oder `Apache Commons Lang` verwenden.

Die konkreten Implementierungsdetails beinhalten die Verwendung der `format` Methode von `DateTimeFormatter`, wobei die Formatzeichenkette die formattierte Darstellung des `LocalDate` Objekts bestimmt.

## Siehe auch
1. Offizielle Java-Dokumentation zu DateTimeFormatter: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
2. Joda-Time Bibliothek: https://www.joda.org/joda-time/
3. Apache Commons Lang: https://commons.apache.org/proper/commons-lang/

---