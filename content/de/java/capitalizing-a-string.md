---
title:    "Java: Strings großschreiben"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es viele kleine Details, die manchmal übersehen werden können. Eine dieser Kleinigkeiten ist die Großschreibung von Zeichenketten. Warum sollte man sich überhaupt damit beschäftigen, eine Zeichenkette zu groß zu schreiben? Nun, es gibt tatsächlich einige Szenarien, in denen dies notwendig sein könnte. Zum Beispiel, wenn man eine Benutzereingabe erhält und sicherstellen möchte, dass diese immer in Großbuchstaben angezeigt wird. Oder wenn man ein Textverarbeitungsprogramm schreibt und eine Funktion benötigt, um die Überschriften in einem Dokument automatisch groß zu schreiben. Im Folgenden zeigen wir, wie man dies in Java erreichen kann.

## Wie man eine Zeichenkette in Java groß schreibt

Um eine Zeichenkette in Java groß zu schreiben, gibt es verschiedene Ansätze. Ein einfacher Weg ist die Verwendung der `toUpperCase()` Methode. Diese Methode wandelt alle Buchstaben in der Zeichenkette in Großbuchstaben um. Hier ist ein Beispiel:

```Java
String text = "hallo welt";
String capitalizedText = text.toUpperCase();
System.out.println(capitalizedText);
```

Die Ausgabe dieses Codes wird sein:

```
HALLO WELT
```

Ein weiterer Ansatz ist die Verwendung der `StringBuilder` Klasse. Hier muss man die Zeichenkette in einen `StringBuilder` umwandeln, die einzelnen Zeichen durchgehen und jedes Zeichen auf die entsprechende Großbuchstaben-Variante ändern und anschließend den `StringBuilder` zurück in eine Zeichenkette umwandeln. Das sieht folgendermaßen aus:

```Java
String text = "hallo welt";
StringBuilder sb = new StringBuilder(text);
for (int i = 0; i < sb.length(); i++) {
  char c = sb.charAt(i);
  if (Character.isLowerCase(c)) {
    sb.setCharAt(i, Character.toUpperCase(c));
  }
}
String capitalizedText = sb.toString();
System.out.println(capitalizedText);
```

Die Ausgabe dieses Codes wird ebenfalls sein:

```
HALLO WELT
```

## Tiefere Einblicke

Es gibt noch viele andere Möglichkeiten, eine Zeichenkette in Java groß zu schreiben. Man könnte auch eine Regex-Methode verwenden oder eine benutzerdefinierte Implementierung schreiben. Für die meisten Fälle ist jedoch die Verwendung der `toUpperCase()` Methode oder der `StringBuilder` Klasse ausreichend.

## Siehe auch

- [Dokumentation der String-Klasse in Java (auf Deutsch)](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Tutorial für die Verwendung von Regex in Java (auf Deutsch)](https://openbook.rheinwerk-verlag.de/javainsel9/javainsel_07_007.htm#mjce790f8bbf37c6ae37838a98431f4341)
- [Einführung in StringBuilder in Java (auf Deutsch)](https://www.java-forum.org/thema/java-tutorial-java-strings-stringbuilder-und-stringbuffer.187391/)