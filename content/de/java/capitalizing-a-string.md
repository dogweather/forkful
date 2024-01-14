---
title:                "Java: Einen String in Großbuchstaben umwandeln."
simple_title:         "Einen String in Großbuchstaben umwandeln."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist ein grundlegendes Konzept in der Java-Programmierung, das oft verwendet wird, um die Darstellung von Texten zu ändern. Es ist wichtig zu verstehen, wie man Strings richtig in Java kapitalisiert, um das gewünschte Ergebnis zu erzielen.

## Wie geht's?

Das Kapitalisieren von Strings kann auf verschiedene Arten erfolgen, je nach den Anforderungen der Anwendung. Eine einfache Möglichkeit ist die Verwendung der `toUpperCase()` und `toLowerCase()` Methoden, um einen String vollständig in Groß- oder Kleinbuchstaben zu konvertieren. Hier ist ein Beispielcode, der einen String in Großbuchstaben umwandelt:

```Java
String text = "Hallo, Welt!";
String capitalized = text.toUpperCase();

System.out.println(capitalized);
// Output: HALLO, WELT!
```

Wenn Sie nur den ersten Buchstaben eines Strings großschreiben möchten, können Sie die `substring()` Methode verwenden, um den Teil des Strings mit dem ersten Buchstaben zu extrahieren und dann `toUpperCase()` darauf anzuwenden. Hier ist ein Beispiel:

```Java
String text = "guten morgen, leute!";
String firstLetter = text.substring(0, 1).toUpperCase();
String restOfString = text.substring(1).toLowerCase();
String capitalized = firstLetter + restOfString;

System.out.println(capitalized);
// Output: Guten morgen, leute!
```

## Tiefergehende Informationen

Das Kapitalisieren von Strings erfordert auch ein Verständnis von Unicode und der verschiedenen Sprachen und Zeichensätze, die von Java unterstützt werden. Zum Beispiel kann die Konvertierung eines Strings, der chinesische, kyrillische oder arabische Zeichen enthält, unterschiedliche Ergebnisse liefern, je nachdem welche Codierung verwendet wird.

Eine weitere wichtige Sache zu beachten ist, dass Strings in Java unveränderlich sind, was bedeutet, dass jede Operation, die an einem String vorgenommen wird, ein neues String-Objekt erzeugt. Dies hat Auswirkungen auf die Effizienz des Kapitalisierens von Strings und die Verwendung von Speicher.

## Siehe auch

- [Java String Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Unicode-Tutorial](https://www.tutorialspoint.com/java/java_unicode_tutorial.htm)
- [String immutability in Java](https://www.baeldung.com/java-immutable-strings)