---
title:                "Java: Die Länge eines Strings finden"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich damit beschäftigen, die Länge eines Strings zu finden? Die Antwort ist einfach: In der Welt der Programmierung gibt es eine Vielzahl an Anwendungsfällen, in denen man die Länge eines Strings benötigt. Zum Beispiel kann es hilfreich sein, die maximale Anzahl an Zeichen in einem Feld oder Textfeld zu begrenzen oder die Eingabe des Nutzers auf eine bestimmte Länge zu überprüfen.

# Wie man die Länge eines Strings in Java findet

Um die Länge eines Strings in Java zu finden, gibt es eine eingebaute Methode namens "length()". Diese Methode gibt die Anzahl der Zeichen in einem String zurück. Schauen wir uns ein Beispiel an:

```Java
String text = "Hallo Welt!";
int length = text.length();
System.out.println("Die Länge des Strings beträgt: " + length);
```

Die Ausgabe dieses Codes wird sein: "Die Länge des Strings beträgt: 12". Bitte beachte, dass Leerzeichen ebenfalls als Zeichen gezählt werden.

# Tiefergehende Informationen

Die Methode "length()" verwendet intern einen sogenannten "String-Buffer", um die Länge des Strings zu berechnen. Ein "String-Buffer" ist ein Datenstruktur, die flexibel die Größe eines Strings ändern kann. Dies ermöglicht es der Methode, die Länge eines Strings in O(1) Zeit (konstante Zeit) zu berechnen, unabhängig von der Länge des Strings.

In Java gibt es auch die Möglichkeit, mit dem ASCII-Code zu arbeiten, um die Länge eines Strings zu bestimmen. Der ASCII-Code ist ein numerisches Codeschema, das jedem Zeichen in der Tastatur eine Zahl zuweist. Die ASCII-Werte der einzelnen Zeichen in einem String werden summiert, um die Länge des Strings zu bestimmen. Dies ist jedoch ein etwas komplexerer Ansatz und wird in den meisten Fällen nicht benötigt.

# Siehe auch

- [Java String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [ASCII code](https://www.ascii-code.com/)
- [Java built-in methods](https://docs.oracle.com/javase/tutorial/java/data/builtinclasses.html) (auf Deutsch)