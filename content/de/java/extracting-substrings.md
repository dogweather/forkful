---
title:    "Java: Extrahieren von Teilzeichenketten"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Warum

Das Extrahieren von Unterzeichenketten ist eine wichtige Fähigkeit beim Programmieren in Java. Es ermöglicht uns, Teile von Zeichenketten zu isolieren und auf sie zuzugreifen, was für verschiedene Anwendungen nützlich sein kann. In diesem Blog-Beitrag werden wir uns genauer ansehen, warum es wichtig ist, Unterzeichenketten zu extrahieren und wie wir dies in Java tun können.

# Wie geht das?

Um Substrings in Java zu extrahieren, verwenden wir die `substring()`-Methode. Diese Methode nimmt zwei Parameter an, den Startindex und den Endindex, und gibt den entsprechenden Teil der Zeichenkette zurück. Zum Beispiel:

```Java
String switzerland = "Schweizerische Eidgenossenschaft";
String ausgabe = switzerland.substring(12, 23);
System.out.println(ausgabe);
```
Die Ausgabe wäre `Eidgenossenschaft`, da wir die Unterzeichenkette von Index 12 bis Index 23 extrahiert haben.

Alternativ können wir auch die `substring()`-Methode mit nur einem Parameter verwenden, der den Startindex angibt. In diesem Fall wird die restliche Zeichenkette ab diesem Index bis zum Ende zurückgegeben. Zum Beispiel:

```Java
String italy = "Repubblica Italiana";
String ausgabe = italy.substring(10);
System.out.println(ausgabe);
```
Die Ausgabe wäre `Italiana`, da wir die Unterzeichenkette ab Index 10 extrahiert haben.

Wir können auch herausfinden, an welcher Position sich ein bestimmtes Zeichen in einer Zeichenkette befindet, indem wir die `indexOf()`-Methode verwenden. Diese Methode gibt den Index des ersten Vorkommens des angegebenen Zeichens zurück. Zum Beispiel:

```Java
String germany = "Bundesrepublik Deutschland";
int position = germany.indexOf('r');
System.out.println(position);
```
Die Ausgabe wäre `9`, da das erste Vorkommen des Buchstabens "r" an Index 9 ist.

# Tiefer eintauchen

Die `substring()`-Methode ist in Java sehr vielseitig und kann auf verschiedene Arten verwendet werden, um Zeichenketten zu manipulieren. Wir können zum Beispiel auch negative Parameter angeben, um die Unterzeichenkette von hinten zu extrahieren. Wenn wir `-3` als Startindex angeben, wird die letzten drei Zeichen der Zeichenkette zurückgegeben. Wenn wir jedoch `-3` als Endindex angeben, wird die Unterzeichenkette ab dem dritten Zeichen von hinten bis zum Ende zurückgegeben.

Wir können auch die `indexOf()`-Methode in Kombination mit `substring()` verwenden, um bestimmte Teile einer Zeichenkette zu finden und zu extrahieren. Zum Beispiel könnten wir den Anfang einer Email-Adresse extrahieren, indem wir den Index des "@"-Zeichens finden und dann `substring()` verwenden, um alle Zeichen davor zurückzugeben.

Es ist wichtig, dass wir bei der Verwendung von `substring()` und `indexOf()` darauf achten, dass wir die richtigen Indizes angeben, da sonst unerwartete Ergebnisse auftreten können. Es könnte auch hilfreich sein, die Dokumentation sorgfältig durchzulesen oder einige Übungen durchzuführen, um das Extrahieren von Unterzeichenketten besser zu verstehen.

# Siehe auch

- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Substring Tutorial](https://www.w3schools.com/java/java_ref_string.asp)
- [Index Of Tutorial](https://www.javatpoint.com/java-string-indexof)