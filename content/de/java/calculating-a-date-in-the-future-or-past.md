---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Java: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Berechnung von zukünftigen oder vergangenen Datum auseinandersetzen? Ganz einfach, weil es in vielen Anwendungsfällen notwendig ist, ein bestimmtes Datum zu bestimmen, sei es für die Planung von Terminen oder das Durchführen von Berechnungen.

## Wie es geht

Die Berechnung von zukünftigen oder vergangenen Date kann in Java auf verschiedene Weise erfolgen. Hier sind einige Beispiele:

```java
// Datum in der Zukunft berechnen
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusYears(2); // Datum in zwei Jahren
System.out.println(futureDate); // Ausgabe: 2023-09-23 (das Datum kann je nach Ausführung variieren)

// Datum in der Vergangenheit berechnen
LocalDate today = LocalDate.now();
LocalDate pastDate = today.minusMonths(6); // Datum vor 6 Monaten
System.out.println(pastDate); // Ausgabe: 2021-03-23 (das Datum kann je nach Ausführung variieren)

// Datum aus String parsen und berechnen
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy"); // Format des Datums angeben
String date = "15-12-2021";
LocalDate parsedDate = LocalDate.parse(date, formatter); // Datum aus dem String parsen
LocalDate calculatedDate = parsedDate.plusWeeks(2); // Zwei Wochen zum Datum hinzufügen
System.out.println(calculatedDate); // Ausgabe: 2022-01-05
```

## Deep Dive

Die Berechnung von Datum in Java basiert auf der Klasse `LocalDate` aus dem java.time Paket. Diese Klasse bietet verschiedene Methoden zum Hinzufügen oder Subtrahieren von Jahren, Monaten, Wochen und Tagen. Es ist auch möglich, ein Datum aus einem String mit einem definierten Format zu parsen.

Es gibt auch andere Klassen, die für die Arbeit mit Datum in Java verwendet werden können, wie z.B. `LocalDateTime` für Datum und Uhrzeit oder `Period` für die Berechnung von Zeitdifferenzen.

## Siehe auch

Für weitere Informationen zur Arbeit mit Datum in Java können Sie diese Links besuchen:

- [Java 16 Dokumentation: java.time Paket](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)
- [Tutorial: Java Datum und Uhrzeit](https://www.w3schools.com/java/java_date.asp)
- [JavaDoc: LocalDate Klasse](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/LocalDate.html)