---
title:                "Java: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist ein grundlegender Teil der Java-Programmierung, da es uns ermöglicht, die Größe eines Strings zu ermitteln und ihn entsprechend zu manipulieren.

# Wie man das macht

Um die Länge eines Strings in Java zu finden, gibt es eine eingebaute Methode namens `length()`, die auf jedem String-Objekt aufgerufen werden kann. Diese Methode gibt einen ganzzahligen Wert zurück, der die Anzahl der Zeichen im String repräsentiert.

```Java
String name = "Katharina";
int length = name.length();
System.out.println(length);
```

**Output:** 10

In diesem Beispiel verwenden wir die `length()` Methode, um die Länge des Strings "Katharina" zu finden und sie dann mit `System.out.println()` auszugeben.

# Tiefere Einblicke

Intern funktioniert die `length()` Methode, indem sie den internen `char` Array des Strings durchläuft und die Anzahl der vorhandenen Zeichen zählt. Es wird empfohlen, diese Methode zu verwenden, um die Länge eines Strings zu ermitteln, anstatt manuell durch den String zu iterieren.

Ein wichtiger Punkt zu beachten ist, dass die `length()` Methode die Leerzeichen im String als Zeichen mitzählt. Zum Beispiel hat der String "Hallo Welt" eine Länge von 11, da das Leerzeichen auch als ein Zeichen gezählt wird.

# Siehe auch

- [Java String class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java String length() method documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#length())