---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist ein übliches Verfahren zum Extrahieren und Speichern von Datumswerten aus Textformaten, besonders beim Umgang mit Benutzereingaben oder Datenfeeds. Es ist nützlich, da Datumsangaben in unterschiedlichen Formaten kommen können und wir müssen sie in einer einheitlichen Form speichern, damit wir sie in unserem Code einfach verwalten können.

## Wie geht's:

In Java verwenden wir die `parse()` Methode in der `SimpleDateFormat` Klasse, um ein Datum aus einem String zu parsen.

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        try {
            SimpleDateFormat format = new SimpleDateFormat("dd.MM.yyyy");
            Date date = format.parse("01.12.2020");
            System.out.println(date);
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}
```
Ausgabe:
```Java
Tue Dec 01 00:00:00 CET 2020
```

## Tiefgang:

Historisch waren die Datumsformate in unterschiedlichen Kulturen und Technologien sehr unterschiedlich. In den frühen Tagen der Programmierung mussten Programmierer oftmals ihre eigenen Parsertools schreiben. Heutzutage stellen fast alle Programmiersprachen eine eingebaute Methode zum Parsen von Datumsstrings bereit. In Java haben wir mehrere Alternativen wie `LocalDate.parse()` oder die `DateTimeFormatter` Klasse.

Intern wird die `parse()` Methode von `SimpleDateFormat` durch Umwandlung des gegebenen Strings in seine Datums-/Zeitfeldwerte implementiert. Diese Werte werden dann in eine `Date`-Instanz überführt, die das spezifische Datum und die Uhrzeit repräsentiert.

## Siehe auch:

Weitere Informationen zu diesem Thema finden Sie unter den folgenden Links:
- Offizielle Java-Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
- Tutorial für Fortgeschrittene zur SimpleDateFormat-Klasse: https://www.javatpoint.com/java-simpledateformat