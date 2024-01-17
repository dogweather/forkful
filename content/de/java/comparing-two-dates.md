---
title:                "Vergleich von zwei Datumsangaben"
html_title:           "Java: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist eine gängige Aufgabe in der Programmierung. Es bezieht sich darauf, zwei Datumsangaben zu vergleichen und zu bestimmen, welches Datum früher oder später ist. Programmierer tun dies, um ihre Anwendungen in Bezug auf Zeit und Datum zu steuern.

## So geht's:
Es gibt verschiedene Möglichkeiten, zwei Daten in Java zu vergleichen. Eine Möglichkeit ist die Verwendung der Methode `compareTo()` der Klasse `LocalDate`, die in der Java 8 API enthalten ist. Diese Methode vergleicht zwei LocalDate-Objekte und gibt einen Wert zurück, der anzeigt, welches Datum früher oder später ist. Hier ist ein Beispiel:

```Java
// Erstellen und Vergleichen von zwei LocalDate-Objekten
LocalDate date1 = LocalDate.of(2021, Month.JANUARY, 1);
LocalDate date2 = LocalDate.of(2021, Month.MARCH, 1);

// Vergleich mit Hilfe der compareTo()-Methode
int result = date1.compareTo(date2);

// Ausgabe des Ergebnisses
System.out.println(result);
```

Die Ausgabe dieses Codes ist `"-2"`, da das Datum 1. Januar 2021 früher ist als das Datum 1. März 2021.

Eine andere Möglichkeit, zwei Daten zu vergleichen, ist die Verwendung der Methode `isBefore()` der Klasse `LocalDate`. Diese Methode gibt einen booleschen Wert zurück, der angibt, ob das von uns übergebene Datum vor dem in der Methode angegebenen Datum liegt. Hier ist ein Beispiel:

```Java
// Erstellen und Vergleichen von zwei LocalDate-Objekten
LocalDate date1 = LocalDate.of(2021, Month.JANUARY, 1);
LocalDate date2 = LocalDate.of(2021, Month.MARCH, 1);

// Vergleich mit Hilfe der isBefore()-Methode
boolean result = date1.isBefore(date2);

// Ausgabe des Ergebnisses
System.out.println(result);
```

Die Ausgabe dieses Codes ist `"true"`, da das Datum 1. Januar 2021 vor dem Datum 1. März 2021 liegt.

## Tiefergehende Informationen:
Das Vergleichen von Daten hat eine lange Geschichte in der Programmierung. In älteren Programmiersprachen wie COBOL mussten Daten in einem speziellen Format gespeichert werden, damit sie verglichen werden konnten. Mit der Einführung von objektorientierter Programmierung und der Java-API wurden jedoch praktischere und einfachere Methoden zur Verfügung gestellt. Einige Alternativen zur Verwendung von `compareTo()` und `isBefore()` sind die Verwendung von `isEqual()` und `isAfter()` oder die Verwendung von `before()` und `after()` aus der `Calendar`-Klasse.

## Siehe auch:
- [Java 8 API-Dokumentation zu LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java-Artikel zu Vergleichsoperatoren](https://www.javatpoint.com/java-comparison-operators)