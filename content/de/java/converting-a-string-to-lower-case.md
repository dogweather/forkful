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

# Was ist es und warum?

Die Konvertierung eines Strings in Kleinbuchstaben ist ein häufiger Schritt in der Java-Programmierung, bei dem alle Buchstaben eines Strings in Kleinbuchstaben umgewandelt werden. Dies ist nützlich, um Vergleiche oder Suche nach Zeichenketten einfacher und konsistenter zu gestalten.

# Wie geht es?

```Java
String str = "HALLO WELT";
String lowerCaseStr = str.toLowerCase();

System.out.println(lowerCaseStr); // gibt "hallo welt" aus
```

# Tiefere Einblicke

Historisch gesehen wurde die Methode zur Konvertierung von Strings in Kleinbuchstaben in Java ursprünglich von der Klasse `java.util.Locale` bereitgestellt. Seit Java 1.2 ist die Methode jedoch in die Klasse `java.lang.String` verschoben worden.

Alternativ können Programmierer die Methode `Character.toLowerCase()` verwenden, um einzelne Buchstaben in Kleinbuchstaben zu konvertieren. Um eine Zeichenkette in Kleinbuchstaben zu konvertieren, müssten Programmierer jedoch jeden Buchstaben in einer Schleife durchlaufen und einzeln konvertieren.

Bei der Implementierung der Methode `toLowerCase()` wird jeder Buchstabe anhand der Unicode-Tabelle in seinen entsprechenden Kleinbuchstaben konvertiert. Es werden keine Sonderzeichen oder Zahlen in Kleinbuchstaben konvertiert, daher bleibt die Reihenfolge dieser Zeichen im ursprünglichen String erhalten.

# Siehe auch

Weitere Informationen zur Verwendung von Strings in Java finden Sie in der offiziellen Oracle-Dokumentation unter der [Kategorie String](https://docs.oracle.com/javase/tutorial/java/data/strings.html).