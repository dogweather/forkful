---
title:                "Java: Vergleich von zwei Daten"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Es ist oft unerlässlich, zwei Daten in der Java-Programmierung zu vergleichen, besonders wenn es um die Überprüfung von Eingaben oder die Sortierung von Daten geht. Das Vergleichen von Daten kann dabei helfen, die Logik und Funktionalität in Ihrem Programm zu verbessern und Fehler zu vermeiden.

## Wie man zwei Daten vergleicht

In Java gibt es verschiedene Möglichkeiten, um zwei Daten zu vergleichen. Eine Möglichkeit ist die Verwendung der "compareTo" Methode, die für alle Datentypen verfügbar ist, die das Interface "Comparable" implementieren. Diese Methode vergleicht zwei Daten und gibt entweder einen positiven, negativen oder Null-Wert zurück, je nachdem ob das erste Daten größer, kleiner oder gleich dem zweiten Datum ist.

Beispiel:

```Java
String date1 = "12/01/2020";
String date2 = "12/05/2020";

int result = date1.compareTo(date2);
System.out.println(result);
```

Dieser Code würde einen negativen Wert zurückgeben, da "date1" vor "date2" liegt. Wenn die Daten gleich sind, gibt die "compareTo" Methode einen Wert von Null zurück.

Eine weitere Möglichkeit ist die Verwendung der "before" und "after" Methoden der "Date" Klasse. Diese Methoden vergleichen direkt zwei Daten und geben einen Boolean-Wert zurück, der angibt, ob das erste Datum vor oder nach dem zweiten Datum liegt.

Beispiel:

```Java
Date date1 = new Date(2020, 01, 12);
Date date2 = new Date(2020, 05, 12);

boolean isBefore = date1.before(date2);
System.out.println(isBefore);
```

Dieser Code würde "true" zurückgeben, da "date1" vor "date2" liegt.

## Tiefere Einblicke

Bei der Verwendung der "compareTo" Methode ist es wichtig zu beachten, dass die Ergebnisse vom Datentyp abhängen. Wenn Sie beispielsweise String-Werte vergleichen, werden die Ergebnisse alphabetisch angeordnet sein, während numerische Werte entsprechend ihrer Größe verglichen werden.

Auch bei der Verwendung der "Date" Klasse gibt es einige Dinge zu beachten. Beim Vergleichen von zwei Daten müssen Sie sicherstellen, dass die Daten auf dieselbe Weise initialisiert werden, da die Standardeinstellung für das Jahr in Java 1900 ist. Wenn Sie auch die Zeit in Ihrem Vergleich berücksichtigen möchten, können Sie die "getTime" Methode verwenden, um die Millisekunden seit Januar 1970 zu erhalten und diese zu vergleichen.

## Siehe auch

- [Oracle Dokumentation zu compareTo](https://docs.oracle.com/javase/7/docs/api/java/lang/Comparable.html)
- [Beispiele für das Vergleichen von Daten in Java](https://www.geeksforgeeks.org/comparing-dates-java/)
- [Java Date Klasse](https://www.tutorialspoint.com/java/java_date_time.htm)