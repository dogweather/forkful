---
title:    "Java: Vergleich von zwei Datum."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten kann in der Programmierung häufig vorkommen, insbesondere wenn es um die Verarbeitung von Daten oder die Überprüfung von Bedingungen geht. Daher ist es wichtig zu wissen, wie man zwei Daten in Java vergleichen kann, um effiziente und fehlerfreie Programme zu schreiben.

# Wie man zwei Daten in Java vergleicht

Um zwei Daten in Java zu vergleichen, gibt es verschiedene Ansätze, abhängig von den Anforderungen des Programms. Ein üblicher Ansatz ist die Verwendung der Methode `compareTo()`, die in der Klasse `Date` definiert ist.

```
Java
Date date1 = new Date(2021, 1, 1); // Erstes Datum
Date date2 = new Date(2022, 1, 1); // Zweites Datum
int result = date1.compareTo(date2); // Vergleich der beiden Daten
```

Der Rückgabewert der `compareTo()` Methode ist ein integer, der 0, positiv oder negativ sein kann, abhängig von der Reihenfolge der Daten. Wenn der Rückgabewert 0 ist, dann sind beide Daten gleich. Wenn der Rückgabewert positiv ist, dann ist das erste Datum später als das zweite. Und wenn der Rückgabewert negativ ist, dann ist das erste Datum früher als das zweite.

Es ist wichtig zu beachten, dass das Vergleichen von Daten nicht nur auf Jahresbasis erfolgt, sondern auch auf Monats- und Tagesbasis. Dies bedeutet, dass ein Datum im Jahr 2022 als später als ein Datum im Jahr 2021 betrachtet wird, auch wenn beide Daten im selben Monat liegen.

# Tiefergehende Information

Es gibt noch eine weitere Möglichkeit, um Daten in Java zu vergleichen, nämlich mit der Methode `equals()`. Diese Methode vergleicht auf Gleichheit und gibt einen boolean Wert zurück. Im Gegensatz zur `compareTo()` Methode, die auf der Reihenfolge basiert, vergleicht `equals()` die tatsächlichen Datenwerte.

```
Java
Date date1 = new Date(2021, 1, 1); // Erstes Datum
Date date2 = new Date(2022, 1, 1); // Zweites Datum
boolean result = date1.equals(date2); // Vergleich der beiden Daten
```

Ein wichtiger Aspekt beim Vergleichen von Daten ist die Zeitzone. In Java kann die Standardzeitzone einen Einfluss auf das Ergebnis des Vergleichs haben. Es ist daher empfehlenswert, die Methode `compareTo()` zu verwenden, die auf UTC basiert und somit unabhängig von der Zeitzone genauere Ergebnisse liefert.

# Siehe auch

- [Java-Dokumentation - Klasse Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Tutorialspoint - Date in Java](https://www.tutorialspoint.com/java/java_date_time.htm)
- [GeeksforGeeks - Comparing Dates in Java](https://www.geeksforgeeks.org/comparing-dates-java/)