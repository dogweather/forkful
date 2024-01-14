---
title:                "Java: Unterschiedliche Unterteile extrahieren"
simple_title:         "Unterschiedliche Unterteile extrahieren"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung gibt es oft Situationen, in denen wir nur einen Teil eines Textes oder einer Zeichenkette brauchen. Zum Beispiel, wenn wir einen Datensatz mit einer bestimmten Formatierung haben und nur bestimmte Informationen davon extrahieren wollen. In solchen Fällen ist es nützlich, Substrings zu extrahieren. Das bedeutet, dass wir Teile einer Zeichenkette auswählen und in einer neuen Variable speichern, um sie später zu verwenden.

## Wie es geht
Das Extrahieren von Substrings in Java ist sehr einfach. Wir verwenden die Methode `substring()` mit dem Start- und Endindex der Zeichenkette, die wir extrahieren möchten. Hier ist ein Beispiel, um die letzten 5 Zeichen einer Zeichenkette zu extrahieren:

```Java
String text = "Willkommen bei meinem Blog!";
String substring = text.substring(text.length() - 5, text.length());
System.out.println(substring); // gibt "Blog!" aus
```

Hier haben wir die Methode `substring()` aufgerufen und den Startindex auf `text.length() - 5` und den Endindex auf `text.length()` gesetzt. Dies bedeutet, dass wir die letzten 5 Zeichen von `text` extrahieren möchten.

Eine weitere nützliche Methode ist `substring()` ohne den Endindex. In diesem Fall werden alle Zeichen ab dem angegebenen Startindex bis zum Ende der Zeichenkette extrahiert. Hier ist ein Beispiel:

```Java
String text = "Morgen ist Dienstag.";
String substring = text.substring(11);
System.out.println(substring); // gibt "Dienstag." aus
```

In diesem Beispiel haben wir die Methode `substring()` aufgerufen und den Startindex auf 11 gesetzt, was bedeutet, dass wir alle Zeichen ab dem 11. Zeichen, also "Dienstag.", extrahieren möchten.

## Tiefergehende Einblicke
Beim Extrahieren von Substrings gibt es einige wichtige Dinge zu beachten. Zunächst einmal ist es wichtig, dass der Startindex kleiner oder gleich dem Endindex ist, sonst wird ein Fehler auftreten. Außerdem ist es immer eine gute Idee, die Länge der Zeichenkette zu überprüfen, bevor wir einen Substring extrahieren, um sicherzustellen, dass der Endindex nicht größer ist als die tatsächliche Länge der Zeichenkette.

Eine weitere nützliche Methode für das Extrahieren von Substrings ist `indexOf()`. Diese Methode gibt den ersten Index eines bestimmten Zeichens oder einer Zeichenkette in einer anderen Zeichenkette zurück. Wir können diese Methode nutzen, um den Startindex für `substring()` automatisch zu berechnen. Hier ist ein Beispiel:

```Java
String text = "Die Tabelle hat 5 Spalten.";
String substring = text.substring(text.indexOf("5"));
System.out.println(substring); // gibt "5 Spalten." aus
```

In diesem Beispiel suchen wir nach dem Zeichen "5" in der Zeichenkette und verwenden `indexOf()` um den Index dieser Zeichenkette zu erhalten. Dann nutzen wir diesen Index, um den Substring ab diesem Index zu extrahieren. Diese Methode ist besonders nützlich, wenn wir keine festen Start- und Endindizes haben, sondern nach bestimmten Mustern oder Zeichenketten suchen möchten.

## Siehe auch
- [Java String Class Documentation](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [Tutorial: CharAt, Substring, and IndexOf Methods in Java](https://www.baeldung.com/java-char-substring-index)