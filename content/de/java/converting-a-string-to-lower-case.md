---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Java: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren eines Strings in Kleinbuchstaben ist eine häufig verwendete Funktion in der Programmierung. Es kann hilfreich sein, um Strings zu vergleichen oder um sicherzustellen, dass Eingaben in einer bestimmten Form vorliegen.

## Wie geht das
Um einen String in Kleinbuchstaben zu konvertieren, gibt es in Java die Methode `toLowerCase()`, die auf einem String-Objekt aufgerufen werden kann. Hier ist ein Beispielcode und die entsprechende Ausgabe:

```Java
String name = "Max Mustermann";
String nameLower = name.toLowerCase();
System.out.println(nameLower);

// Ausgabe: max mustermann
```

In diesem Beispiel wird die `toLowerCase()`-Methode auf dem String `name` aufgerufen und das Ergebnis in der Variable `nameLower` gespeichert. Das Ergebnis des Aufrufs ist der String in Kleinbuchstaben.

## Tiefgehende Einblicke
Bei der Konvertierung eines Strings in Kleinbuchstaben gibt es einige Dinge zu beachten. Zum einen ist es wichtig zu wissen, dass die `toLowerCase()`-Methode nur Buchstaben in Großbuchstaben in Kleinbuchstaben umwandelt. Alle anderen Zeichen bleiben unverändert. Zum Beispiel würde aus dem String `"Hallo Welt!"` nach der Konvertierung `"hallo welt!"` werden.

Ein weiterer wichtiger Punkt ist, dass die `toLowerCase()`-Methode nur die Standard-Kleinbuchstaben des entsprechenden Zeichensatzes verwendet. Das bedeutet, dass je nach Sprache und Zeichensatz die konvertierten Kleinbuchstaben variieren können. Deshalb sollte man bei der Verwendung dieser Methode darauf achten, dass sie für die jeweilige Anwendung geeignet ist.

Eine Alternative zur `toLowerCase()`-Methode ist die Verwendung der `String`-Klasse und ihrer `toLowerCase()`-Methode. Diese Methode verwendet den Standard-Locale des Systems, um die Kleinbuchstaben zu bestimmen, was zu genaueren Ergebnissen führt.

## Siehe auch
- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String.toLowerCase() Method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java Locale Class](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)