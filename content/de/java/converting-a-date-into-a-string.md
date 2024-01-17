---
title:                "Eine Datum in eine Zeichenkette umwandeln"
html_title:           "Java: Eine Datum in eine Zeichenkette umwandeln"
simple_title:         "Eine Datum in eine Zeichenkette umwandeln"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Was & Warum?
Die Umwandlung eines Datums in einen String ist ein gängiges Vorgehen in der Programmierung. Dabei wird ein Datum, das als numerischer Wert gespeichert ist, in einen lesbaren Text umgewandelt. Programmierer tun dies, um die Ausgabe von Datumsangaben in einem bestimmten Format zu kontrollieren oder um die Lesbarkeit von Code zu verbessern.

Wie geht das?
In Java gibt es verschiedene Möglichkeiten, ein Datum in einen String umzuwandeln. Eine einfache Methode ist die Verwendung der ```toString()``` Methode für Datumsklassen wie ```Date``` oder ```Calendar```. Diese gibt das Datum in einem festgelegten Format zurück, abhängig von der lokalen Einstellung des Systems. Beispielcode:

```
// Datumsobjekt erstellen
Date date = new Date();
// Datumsobjekt in String umwandeln
String dateString = date.toString();
// Ausgabe
System.out.println(dateString);
```
**Ausgabe:**

```Mon Sep 20 10:30:15 CEST 2021```

Eine weitere Möglichkeit ist die Verwendung von Formatierungsmustern mithilfe der ```SimpleDateFormat``` Klasse. Diese erlaubt es, das Datum in einem spezifischen Format auszugeben. Beispielcode:

```
// Datumsobjekt erstellen
Date date = new Date();
// Formatierungsmuster erstellen
SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
// Datumsobjekt in String umwandeln und formatieren
String dateString = sdf.format(date);
// Ausgabe
System.out.println(dateString);
```
**Ausgabe:**

```20.09.2021```

Tiefer graben
Die Umwandlung von Datum in String hat eine lange Geschichte in der Softwareentwicklung. Früher mussten Entwickler oft komplexe Berechnungen durchführen, um ein Datum in ein bestimmtes Format zu bringen. Mit der Einführung von Formatierungsmustern und der Standardisierung des Datumsformats hat sich die Umwandlung vereinfacht.

Als Alternative zur Umwandlung in einen String können Programmierer auch auf die ```Date.toInstant()``` und ```Instant.toString()``` Methode zurückgreifen, um das Datum in einen lesbaren Text umzuwandeln. Zudem gibt es in Java die Möglichkeit, die Klasse ```DateTimeFormatter``` zu verwenden, welche ein umfangreiches Set an Formatierungsmustern für Datumsangaben zur Verfügung stellt.

Zur Vertiefung empfiehlt es sich, die Java-Dokumentation zu den genannten Klassen und Methoden zu studieren.

Siehe auch
- [Java-Dokumentation: Datumsklassen](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java-Dokumentation: SimpleDateFormat Klasse](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java-Dokumentation: DateTimeFormatter Klasse](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)