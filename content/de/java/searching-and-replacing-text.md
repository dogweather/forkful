---
title:    "Java: Suchen und Ersetzen von Text"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Suchen und Ersetzen von Text ist ein grundlegender Bestandteil der Java-Programmierung. Es ermöglicht Entwicklern, effizient und präzise Text innerhalb von Strings oder Dateien zu ändern. Weiterlesen, um zu lernen, wie es funktioniert und wie man es in seinen Code integrieren kann.

# Wie geht es

Die zwei Hauptmethoden zur Suche und Ersetzung von Text in Java sind `replace()` und `replaceAll()`. Beide nehmen zwei Argumente, den zu ersetzenden Text und den Ersatztext, und geben eine neue Zeichenfolge zurück.

```Java
String originalString = "Hallo, Welt!";
String newString = originalString.replace("Hallo", "Guten Tag");
System.out.println(newString);

// Output: Guten Tag, Welt!
```

In diesem Beispiel wird der Text "Hallo" durch "Guten Tag" ersetzt. Die Methode `replaceAll()` funktioniert auf die gleiche Weise, verwendet jedoch reguläre Ausdrücke für die Suche nach Textmustern.

```Java
String originalString = "Der Sommer ist die schönste Jahreszeit.";
String newString = originalString.replaceAll("Jahreszeit", "Jahreszeit des Genusses");
System.out.println(newString);

// Output: Der Sommer ist die schönste Jahreszeit des Genusses.
```

# Tiefe Einsicht

Es gibt viele Optionen und Methoden, die bei der Suche und Ersetzung von Text verwendet werden können, einschließlich der Verwendung von regulären Ausdrücken und der Verwendung von anderen Klassen wie `StringBuilder` und `StringBuffer`. Es ist auch wichtig, sich bewusst zu sein, dass die Methoden `replace()` und `replaceAll()` nur die erste Übereinstimmung innerhalb des Textes ersetzen. Um alle Übereinstimmungen zu ersetzen, muss man die Methode `replaceFirst()` oder die Verwendung von Schleifen verwenden.

# Siehe auch

- [Java String-Klasse](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java-Reguläre Ausdrücke](https://docs.oracle.com/javase/tutorial/essential/regex/)