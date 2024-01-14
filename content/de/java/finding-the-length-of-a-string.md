---
title:    "Java: Die Länge einer Zeichenkette finden"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine essentielle Aufgabe beim Programmieren in Java. Oftmals müssen wir die Anzahl der Zeichen in einem String überprüfen oder die Länge eines Strings mit einer anderen Variable vergleichen. Mit der richtigen Methode können wir dies schnell und effizient erreichen.

# Wie geht das?

Um die Länge eines Strings in Java zu finden, gibt es eine praktische integrierte Funktion, die `length()` genannt wird. Diese Funktion gibt die Anzahl der Zeichen in einem String zurück.

```Java
String str = "Hallo, Welt!";
int length = str.length();
System.out.println(length); // Ausgabe: 13
```

In diesem Beispiel wird der `length()` Befehl auf den String "Hallo, Welt!" angewendet. Die Funktion zählt die Anzahl der Zeichen (einschließlich Leerzeichen) und gibt sie als Integer-Wert zurück. Dieser Wert wird dann in der Variable `length` gespeichert und anschließend ausgegeben.

Eine wichtige Sache zu beachten ist, dass das Zählen der Zeichen bei 1 beginnt, nicht bei 0. Das bedeutet, dass der obige String eine Länge von 13 hat, obwohl er nur 12 Buchstaben enthält.

# Eine tiefergehende Erklärung

Die `length()` Funktion verwendet die Unicode-Kodierung, um die Länge eines Strings zu bestimmen. Dies bedeutet, dass sie nicht nur einzelne Buchstaben, sondern auch Sonderzeichen, Emojis und andere nicht-ASCII-Zeichen korrekt zählen kann.

Ein weiteres interessantes Merkmal dieser Funktion ist, dass sie auch bei leeren Strings funktioniert. Ein leerer String hat eine Länge von 0, da keine Buchstaben enthalten sind.

Für weitere fortgeschrittene Möglichkeiten der Längenberechnung, gibt es auch die `codePointCount()` Funktion, die berücksichtigt, dass Zeichen wie emojis mehrere Einheiten im String belegen können.

# Siehe auch

- [Oracle Dokumentation über die `length()` Funktion](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Baeldung's Artikel über die Längenberechnung von Strings](https://www.baeldung.com/java-string-length)