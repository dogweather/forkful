---
title:    "Java: Umwandlung eines Datums in einen String"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Warum: In der Programmierung ist es oft notwendig, ein Datum in einen String (Zeichenkette) umzuwandeln. Dies kann zum Beispiel bei der Ausgabe von Daten in einem bestimmten Format oder bei der Speicherung von Daten in einer Datenbank nützlich sein.

Wie geht es: Um ein Datum in einen String umzuwandeln, können wir in Java die Methode "format()" aus der Klasse "SimpleDateFormat" verwenden. Hier ist ein Beispielcode:

```Java
// Importieren der erforderlichen Klassen
import java.text.SimpleDateFormat;
import java.util.Date;

// Definieren des Datums-Formats
SimpleDateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");

// Erstellen eines Datums-Objekts
Date date = new Date();

// Verwenden der "format()" Methode, um das Datum in einen String umzuwandeln
String dateString = dateFormat.format(date);

// Ausgabe des Ergebnisses
System.out.println("Das aktuelle Datum ist: " + dateString);

// Das aktuelle Datum ist: 20.03.2021
```

Tief tauchen: Es gibt verschiedene Möglichkeiten, ein Datum in einen String umzuwandeln, je nachdem, welches Format und welche Locale (Sprache) wir benötigen. Die Klasse "SimpleDateFormat" bietet zahlreiche Methoden zur Formatierung von Daten und zur Verwendung von verschiedenen Locale-Einstellungen. Es ist auch wichtig zu beachten, dass die Verwendung von statischen Formatierern wie "DateTimeFormatter" in der Klasse "LocalDateTime" eine bessere Leistung bieten kann, insbesondere in multithreaded Umgebungen.

Siehe auch: [Java - Date and Time](https://www.javatutorialhq.com/java/lang/dates-and-times-in-java/), [Java - SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html), [Java - DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)