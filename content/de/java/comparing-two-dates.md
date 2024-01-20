---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Vergleichen von zwei Daten ist ein gewöhnlicher Vorgang in der Programmierung und ermöglicht es uns, zu überprüfen, welches Datum früher oder später ist. Dies ist nützlich in vielen Situationen, wie z.B. beim Sortieren von Ereignissen nach Datum oder beim Berechnen der Dauer zwischen zwei Daten.

## So geht's:

Ein gängiger Weg zum Vergleichen von zwei Daten in Java ist die Verwendung der `compareTo()` Methode aus der `java.time.LocalDate` Klasse.

```Java
import java.time.LocalDate;

public class Main {

  public static void main(String[] args) {

    LocalDate date1 = LocalDate.of(2020, 1, 1);
    LocalDate date2 = LocalDate.of(2021, 1, 1);

    if (date1.compareTo(date2) < 0) {
      System.out.println("Das erste Datum ist früher.");
    } else if (date1.compareTo(date2) > 0) {
      System.out.println("Das erste Datum ist später.");
    } else {
      System.out.println("Beide Daten sind gleich.");
    }

  }
}
```
Die Ausgabe ist:

```
Das erste Datum ist früher.
```

## Vertiefung:

Historisch gesehen hat man Daten in Java mit `java.util.Date` und `java.util.Calendar` verglichen, welche jedoch unter heutiger Betrachtung als fehleranfällig und weniger intuitiv gelten. Deshalb empfiehlt es sich, ab Java 8 die neue API unter `java.time` zu verwenden.

Eine alternative Methode zum Vergleichen von Daten ist die `isBefore()`, `isAfter()`, oder `isEqual()` Methode:

```Java
if (date1.isBefore(date2)) {
  System.out.println("Das erste Datum ist früher.");
} else if (date1.isAfter(date2)) {
  System.out.println("Das erste Datum ist später.");
} else {
  System.out.println("Beide Daten sind gleich.");
}
```
Aus internen Gründen erstellen diese Methoden eine Kopie der Daten, was zu minimal höherem Speicherverbrauch führen kann, aber in den meisten Fällen ist dieser Unterschied vernachlässigbar.

## Siehe auch:

- [Oracles offizielle Dokumentation zur `java.time.LocalDate` Klasse](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Eine Anleitung zu den neuen Datums- und Zeiteinheiten in Java 8](https://www.baeldung.com/java-8-date-time-intro)