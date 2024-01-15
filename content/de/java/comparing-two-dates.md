---
title:                "Vergleich von zwei Daten."
html_title:           "Java: Vergleich von zwei Daten."
simple_title:         "Vergleich von zwei Daten."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann in vielen Fällen sehr hilfreich sein. Zum Beispiel beim Überprüfen von Ablaufdaten oder beim Sortieren von Datensätzen nach Datum. In diesem Artikel lernen wir, wie man mit Java zwei Datumswerte vergleicht.

## So geht's!

Um zwei Datumsobjekte zu vergleichen, können wir die `compareTo()` Methode der `java.util.Date` Klasse verwenden. Diese Methode gibt eine Ganzzahl zurück, die angibt, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt.

```java
Date date1 = new Date(); //erstes Datum
Date date2 = new Date(); //zweites Datum

int result = date1.compareTo(date2); //Ergebnis speichern

if (result == 0) { //wenn das erste Datum gleich dem zweiten Datum ist
    System.out.println("Beide Daten sind identisch.");
} else if (result < 0) { //wenn das erste Datum vor dem zweiten Datum ist
    System.out.println("Das erste Datum liegt vor dem zweiten Datum.");
} else { //wenn das erste Datum nach dem zweiten Datum ist
    System.out.println("Das erste Datum liegt nach dem zweiten Datum.");
}
```
Output: `Beide Daten sind identisch.`

Die `compareTo()` Methode kann auch in Kombination mit anderen Datumsfunktionen wie `Calendar` und `LocalDate` verwendet werden. Hier ist ein Beispiel mit `Calendar`:

```java
Calendar date1 = Calendar.getInstance(); //erstes Datum
Calendar date2 = Calendar.getInstance(); //zweites Datum

date1.set(2020, Calendar.NOVEMBER, 10); //Datum setzen
date2.set(2020, Calendar.NOVEMBER, 11); //Datum setzen

int result = date1.compareTo(date2); //Ergebnis speichern

if (result == 0) { //wenn das erste Datum gleich dem zweiten Datum ist
    System.out.println("Beide Daten sind identisch.");
} else if (result < 0) { //wenn das erste Datum vor dem zweiten Datum ist
    System.out.println("Das erste Datum liegt vor dem zweiten Datum.");
} else { //wenn das erste Datum nach dem zweiten Datum ist
    System.out.println("Das erste Datum liegt nach dem zweiten Datum.");
}
```
Output: `Das erste Datum liegt vor dem zweiten Datum.`

## Tiefer eintauchen

Beim Vergleichen von zwei Datumswerten gibt es mehrere Dinge zu beachten. Zum einen kann es nützlich sein, nur auf das Datum oder nur auf die Uhrzeit zu achten und diese getrennt zu vergleichen. Hierfür können wir die `setTime()` Methode verwenden, um die Uhrzeit auf `0` zu setzen.

```java
Date date1 = new Date(); //erstes Datum
Date date2 = new Date(); //zweites Datum

//nur das Datum vergleichen
date1.setTime(0);
date2.setTime(0);

int result = date1.compareTo(date2); //Ergebnis speichern

//nur die Uhrzeit vergleichen
date1.setTime(date1.getTime() - date1.getTime() % (24 * 60 * 60 * 1000));
date2.setTime(date2.getTime() - date2.getTime() % (24 * 60 * 60 * 1000));

if (result == 0) { //wenn die Uhrzeiten gleich sind
    System.out.println("Die Daten haben die gleiche Uhrzeit.");
} else if (result < 0) { //wenn die Uhrzeit von date1 vor der von date2 liegt
    System.out.println("Die Uhrzeit von date1 liegt vor der von date2.");
} else { //wenn die Uhrzeit von date1 nach der von date2 liegt
    System.out.println("Die Uhrzeit von date1 liegt nach der von date2.");
}
```
Output: `Die Daten haben die gleiche Uhrzeit.`

Eine weitere wichtige Sache beim Vergleichen von zwei Datumswerten ist, dass die `compareTo()` Methode nur für Datumsangaben bis auf Millisekundenebene genau ist. Für eine genauere Vergleichsmöglichkeit können wir die `equals()` Methode der `java.time.LocalDate` Klasse verwenden. Diese vergleicht bis auf die Nanosekundenebene genau.

```java
LocalDate date1 = LocalDate.now(); //erstes Datum
LocalDate date2 = LocalDate.now(); //zweites Datum

if (date1.equals(date2