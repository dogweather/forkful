---
title:                "Java: Vergleich von zwei Datum"
simple_title:         "Vergleich von zwei Datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten ist ein häufiges Szenario in der Java-Programmierung. Es kann verwendet werden, um zu überprüfen, ob eine bestimmte Aktion bereits ausgeführt wurde oder um zu bestimmen, welches Datum früher oder später ist.

# Wie

Um zwei Daten in Java zu vergleichen, können wir die Klasse "Date" verwenden und die Methode "compareTo()". Diese Methode gibt entweder einen positiven, negativen oder neutralen Wert zurück, abhängig davon, ob das erste Datum vor, nach oder gleich dem zweiten Datum ist.

```Java
// Beispiel zur Verwendung von compareTo() für Datum
Date date1 = new Date(2020, 10, 15); // 15. Oktober 2020
Date date2 = new Date(2020, 9, 20); // 20. September 2020

int result = date1.compareTo(date2);

if (result < 0) {
    System.out.println("Date 1 ist vor Date 2");
} else if (result > 0) {
    System.out.println("Date 1 ist nach Date 2");
} else {
    System.out.println("Date 1 und Date 2 sind gleich");
}

// Output: Date 1 ist nach Date 2
```

# Deep Dive

Bei der Verwendung der Methode "compareTo()" gibt es einige wichtige Dinge zu beachten. Zunächst muss eines der beiden Daten vor dem Vergleich mit der Methode "before()" oder "after()" auf Gleichheit überprüft werden. Andernfalls kann es zu ungenauen Vergleichen kommen, da die Methode "compareTo()" auf die Millisekunde genau vergleicht.

Ein weiterer wichtiger Punkt ist, dass die Klasse "Date" veraltet ist und durch die neuen Klassen "LocalDate", "LocalTime" und "LocalDateTime" in der Java 8 API ersetzt wurde. Diese Klassen bieten bessere Methoden zur Vergleichung von Daten und Zeiten.

# Siehe auch

- [Java Date compareTo() Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#compareTo(java.util.Date))
- [Java 8 API Dokumentation](https://docs.oracle.com/javase/8/docs/api/)
- [Vergleich der neuen Date/Time API mit der Date-Klasse](https://docs.oracle.com/javase/tutorial/datetime/overview/legacydatetime.html)