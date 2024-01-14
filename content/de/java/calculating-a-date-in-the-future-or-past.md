---
title:                "Java: Berechnung eines Datums in der Zukunft oder Vergangenheit."
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datumswerten ist ein wichtiger Teil der Programmierung, der in vielen Anwendungsfällen erforderlich ist. Zum Beispiel können Sie Daten in einer Anwendung nach einem bestimmten Zeitraum filtern oder ein automatisiertes System erstellen, das Erinnerungen für bevorstehende Termine sendet. In diesem Blog-Beitrag werden wir uns ansehen, wie man mit Java einfach Datumswerte in der Zukunft oder Vergangenheit berechnet.

## Wie geht das

Die Klasse `LocalDate` aus dem Java 8 Paket `java.time` macht die Arbeit der Berechnung von Datumswerten extrem einfach. Um ein zukünftiges Datum zu berechnen, können Sie die `plus` Methode verwenden und eine bestimmte Anzahl von Jahren, Monaten, Tagen und sogar Wochen angeben. Zum Beispiel:

```Java
// Zwei Jahre in die Zukunft berechnen
LocalDate zukunft = LocalDate.now().plusYears(2);

// Ein Monat und eine Woche in die Zukunft berechnen
LocalDate zukunft = LocalDate.now().plusMonths(1).plusWeeks(1);
```

Um ein Datum in der Vergangenheit zu berechnen, können Sie die `minus` Methode mit denselben Parametern verwenden. Sehen wir uns ein Beispiel an:

```Java
// Ein Jahr und drei Monate in die Vergangenheit berechnen
LocalDate vergangenheit = LocalDate.now().minusYears(1).minusMonths(3);

// Sie können auch negative Werte angeben, um in die Vergangenheit zu gehen
LocalDate vergangenheit = LocalDate.now().minusWeeks(-2);
```

Es ist auch möglich, Datumsbereiche zu berechnen, indem man die `plus` oder `minus` Methode auf ein bereits berechnetes Datum anwendet. Schauen wir uns ein Beispiel dafür an:

```Java
// Aktuelles Datum berechnen
LocalDate heute = LocalDate.now();

// Datum für 5 Jahre in der Zukunft berechnen
LocalDate zukunft = heute.plusYears(5);

// Datumsbereich für die nächsten 30 Tage berechnen
LocalDate in30Tagen = heute.plusDays(30);

// Datumsbereich für die letzten 2 Wochen berechnen
LocalDate vor2Wochen = heute.minusWeeks(2);
```

## Tiefergehende Analyse

Die `LocalDate` Klasse enthält noch viele weitere nützliche Methoden für die Arbeit mit Datumswerten. Beispielsweise können Sie mit der `getDayOfWeek()` Methode den Wochentag eines bestimmten Datums abrufen oder mit der `isLeapYear()` Methode prüfen, ob es sich um ein Schaltjahr handelt.

Es ist auch möglich, ein Datum aus einer Zeichenkette zu parsen und in ein `LocalDate` Objekt umzuwandeln. Hier ist ein Beispiel:

```Java
// Datum aus einer Zeichenkette parsen
String datumZeichenkette = "2021-12-31";
LocalDate datum = LocalDate.parse(datumZeichenkette);

// Das Datum ist jetzt ein LocalDate Objekt
// Sie können nun die oben genannten Methoden anwenden
```

Abschließend ist es wichtig zu beachten, dass das Berechnen von Datumswerten keine triviale Aufgabe ist und es auch Abweichungen in verschiedenen Zeitzonen und Kalendersystemen geben kann. Es ist daher immer ratsam, die Dokumentation der `LocalDate` Klasse und des `java.time` Pakets zu konsultieren, um genau zu verstehen, wie diese Berechnungen durchgeführt werden.

## Siehe auch

- [Java 8 `LocalDate` Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java 8 `java.time` Paket Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java 8 Datum und Uhrzeit Tutorial](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)