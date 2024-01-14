---
title:                "Java: Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Die Berechnung von zukünftigen oder vergangenen Daten ist eine wichtige Fähigkeit in der Programmierung, die in vielen Anwendungsfällen benötigt wird. Es ermöglicht uns, präzise Zeiten für Aufgaben, Ereignisse oder Prozesse zu planen und zu verwalten.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `LocalDate` Klasse aus der Java 8 Date and Time API verwenden. Zuerst müssen wir ein heutiges Datum festlegen, indem wir die `now()` Methode aufrufen:

```Java
LocalDate today = LocalDate.now();
```

Dann können wir mithilfe der `plus()` und `minus()` Methoden ein zukünftiges bzw. vergangenes Datum berechnen. Zum Beispiel, um 4 Tage in die Zukunft zu berechnen, können wir Folgendes tun:

```Java
LocalDate futureDate = today.plusDays(4);
```

Auf ähnliche Weise können wir ein Datum in der Vergangenheit berechnen, indem wir die `minus()` Methode verwenden. Zum Beispiel, um 2 Monate in der Vergangenheit zu berechnen, können wir Folgendes tun:

```Java
LocalDate pastDate = today.minusMonths(2);
```

Wir können auch andere Zeitangaben wie Stunden, Wochen oder Jahre verwenden, um die Berechnung durchzuführen.

## Tiefergehende Informationen

Wenn wir ein detailliertes Datum mit Angaben wie Jahr, Monat und Tag berechnen möchten, können wir auch die `LocalDate.of()` Methode verwenden. Zum Beispiel, um den 20. April 2022 zu berechnen, können wir Folgendes tun:

```Java
LocalDate specificDate = LocalDate.of(2022, 4, 20);
```

Es ist auch möglich, unterschiedliche Zeitzonen und Kalendersysteme zu berücksichtigen, indem wir die entsprechenden Klassen aus der Java 8 Date and Time API verwenden.

## Siehe auch

- [Offizielle Dokumentation von Java 8 zu Date and Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial zu Datum und Zeit in Java 8](https://www.baeldung.com/java-8-date-time-intro)
- [Java-Dokumentation zu LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)