---
title:    "Java: Erhalten des aktuellen Datums"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist oft ein wichtiges Element in der Programmierung. Es kann verwendet werden, um zeitbasierte Aktionen auszuführen oder um eine Zeitstempel für Dateien oder Datenbankeinträge zu erstellen.

## Wie man das aktuelle Datum in Java erhält

Das aktuelle Datum in Java zu erhalten ist einfacher als Sie vielleicht denken. Verwenden Sie einfach die `java.util.Date` Klasse und die `getTime()` Methode, um das aktuelle Datum als Millisekunden seit dem 1. Januar 1970 zurückzugeben. Hier ist ein Beispielcode:

```java
import java.util.Date;

public class DateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        long timestamp = currentDate.getTime();
        System.out.println("Aktuelles Datum: " + currentDate);
        System.out.println("Zeitstempel: " + timestamp);
    }
}
```

Die Ausgabe dieses Codes sieht folgendermaßen aus:

```
Aktuelles Datum: Mon Oct 25 10:30:00 UTC 2021
Zeitstempel: 1635161400000
```

Es ist auch möglich, das aktuelle Datum in einem bestimmten Format auszugeben, indem man die `SimpleDateFormat` Klasse verwendet. Hier ist ein Beispielcode dafür:

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");
        System.out.println("Aktuelles Datum: " + dateFormat.format(currentDate));
    }
}
```

Die Ausgabe dieses Codes lautet:

```
Aktuelles Datum: 25.10.2021
```

## Tiefergehende Informationen über das aktuelle Datum

Obwohl das Erhalten des aktuellen Datums in Java einfach ist, gibt es ein paar Dinge, die Sie im Hinterkopf behalten sollten. Zum Beispiel kann die `java.util.Date` Klasse nur das Datum bis auf die Millisekunde genau darstellen und ist nicht thread-safe. Deshalb empfehlen sich andere Klassen wie `LocalDate` oder `Calendar`, um das Datum genauer und sicherer zu verarbeiten.

Außerdem wurde in Java 8 die neue `java.time` API eingeführt, die viele nützliche Klassen und Methoden zur Verarbeitung von Datums- und Zeitinformationen bietet. Diese API ist auch thread-safe, hat eine bessere Genauigkeit und erleichtert die Arbeit mit Zeitzonen und unterschiedlichen Kalendersystemen.

Insgesamt gibt es also mehrere Möglichkeiten, das aktuelle Datum in Java zu erhalten, je nach Ihren spezifischen Anforderungen und der verwendeten Java-Version.

## Siehe auch

- [Offizielle Dokumentation zu java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java-Kalender und Datum-Beispiel](https://examples.javacodegeeks.com/core-java/util/calendar/java-calendar-and-date-example/)
- [Einführung in die Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)