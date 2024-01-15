---
title:                "Ein String großschreiben"
html_title:           "Java: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In Java geht es oft darum, Text auf verschiedene Weisen zu bearbeiten. Ein häufiger Fall ist das Großschreiben von Strings, um sie in einer bestimmten Form auszugeben. Hier erfährst du, wie du das in Java machen kannst.

## Wie funktioniert das

Um einen String in Java in Großbuchstaben umzuwandeln, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist, die `toUpperCase()`-Methode zu verwenden, die in der String-Klasse verfügbar ist. Hier ist ein Beispiel:

```java
String name = "hallo welt";
String nameUpperCase = name.toUpperCase();

System.out.println(nameUpperCase);
```

Dieses Stück Code verwendet die `toUpperCase()`-Methode, um den String `name` in Großbuchstaben umzuwandeln und dann den neuen String in der Konsole auszugeben. Das Ergebnis ist `HALLO WELT`.

Du kannst auch die `charAt()`- und `setCharAt()`-Methoden verwenden, um jeden Buchstaben im String manuell in einen Großbuchstaben zu ändern. Hier ist ein Beispiel:

```java
String name = "hallo welt";
StringBuilder nameBuilder = new StringBuilder(name);

for (int i = 0; i < name.length(); i++) {
    char currentChar = name.charAt(i);
    char capitalizedChar = Character.toUpperCase(currentChar);
    nameBuilder.setCharAt(i, capitalizedChar);
}

String nameUpperCase = nameBuilder.toString();
System.out.println(nameUpperCase);
```

In diesem Fall nutzen wir eine Schleife, um durch den String `name` zu iterieren und jeden Buchstaben in einen Großbuchstaben zu ändern. Wir erstellen auch einen `StringBuilder`, weil Strings in Java unveränderbar sind und wir daher eine neue Variable benötigen, um unsere Änderungen vorzunehmen.

## Tiefer eintauchen

Wie du sehen kannst, gibt es verschiedene Möglichkeiten, Strings in Großbuchstaben umzuwandeln. Diese Entscheidung hängt oft von der Situation ab, in der du dich befindest. Die `toUpperCase()`-Methode ist eine einfache und schnelle Möglichkeit, den gesamten String auf einmal zu ändern. Die Verwendung von `charAt()` und `setCharAt()` erfordert etwas mehr Code, bietet dir aber auch mehr Kontrolle über jeden einzelnen Buchstaben.

Eine wichtige Sache, die du beachten musst, ist, dass Java ein Mapping verwendet, um Buchstaben in Groß- und Kleinbuchstaben umzuwandeln. Dies bedeutet, dass nicht immer alle Zeichen automatisch in Großbuchstaben umgewandelt werden. Zum Beispiel wird das deutsche "ß" nicht automatisch in ein großes "SS" umgewandelt. Du musst daher möglicherweise manuelle Anpassungen vornehmen, um sicherzustellen, dass alle Zeichen in deinem String in der gewünschten Form sind.

In Java gibt es auch die Möglichkeit, benutzerdefinierte Methoden zu erstellen, die Strings in Großbuchstaben umwandeln können. Dies erfordert etwas fortgeschrittenere Kenntnisse der Sprache, kann aber sehr nützlich sein, wenn du dies in deinem Code an mehreren Stellen tun musst.

## Siehe auch

- [Java String Klasse](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java StringBuilder Klasse](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)