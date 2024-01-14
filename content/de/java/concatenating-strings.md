---
title:    "Java: Verkettung von Zeichenketten"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Warum

Die Verkettung von Strings ist eine wichtige Funktion in der Java-Programmierung, die es ermöglicht, Zeichenketten miteinander zu verbinden. Dies ist besonders hilfreich, um benutzerfreundliche und lesbare Ausgaben zu erzeugen.

# Wie man Strings verkettet

Die Verkettung von Strings in Java ist einfach und erfordert nur die Verwendung des "+" Operators. Im folgenden Code-Beispiel werden zwei Strings miteinander verkettet und die Ausgabe auf der Konsole ausgegeben:

```Java
String firstName = "Max";
String lastName = "Mustermann";

String fullName = firstName + " " + lastName;
System.out.println(fullName);
```

Die Ausgabe dieses Codes wäre "Max Mustermann", da die Strings "firstName" und "lastName" mit einem Leerzeichen dazwischen verbunden wurden.

# Tiefer Einblick

Bei der Verkettung von Strings sollten Sie beachten, dass der "+" Operator Assoziativgesetze folgt, was bedeutet, dass die Ausdrücke von links nach rechts ausgewertet werden. Dies kann zu unerwarteten Ergebnissen führen, insbesondere wenn Zahlen und Strings gemischt werden.

In solchen Fällen können Sie die Klasse "StringBuilder" verwenden, die eine effiziente Möglichkeit bietet, Strings zu verkettet. Im Gegensatz zu dem "+" Operator, der immer neue String-Objekte erzeugt, aktualisiert "StringBuilder" nur ein bestehendes Objekt, was die Leistung verbessert.

# Siehe auch

- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [StringBuilder Klasse](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Umgang mit Strings in Java](https://www.geeksforgeeks.org/java-handle-strings/#:~:text=Strings%20are%20an%20important%20part,concatenating%20two%20strings%20together.&text=Java%20%2B%20operator%20i.e.-%20Strings%20Java)