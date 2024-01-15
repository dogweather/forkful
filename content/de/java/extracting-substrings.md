---
title:                "Teilstrings extrahieren"
html_title:           "Java: Teilstrings extrahieren"
simple_title:         "Teilstrings extrahieren"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Fälle, in denen man in Java mit Text arbeitet und nur einen Teil davon benötigt. Hier kommen Substrings ins Spiel - sie erlauben es uns, Teile eines Strings zu extrahieren und sie für verschiedene Zwecke zu verwenden, z.B. für Stringvergleiche oder zur Weiterverarbeitung.

## Wie Man Es Macht

Die Verwendung von Substrings ist in Java sehr einfach. Beginnen wir mit einem einfachen Beispiel, in dem wir einen Teil eines Strings extrahieren und ihn in einer neuen Variable speichern. Wir nehmen an, dass wir einen vollständigen Namen haben und nur den Nachnamen benötigen:

```Java
String name = "Max Mustermann";
String lastName = name.substring(4); // "Mustermann"
```

Der Aufruf von `substring()` erfolgt auf dem ursprünglichen String und als Parameter geben wir die Startposition des zu extrahierenden Teils an. In diesem Fall beginnt der Nachname bei Index 4 (beginnend bei 0) im Namen. Wir könnten auch eine Start- und Endposition angeben, um einen Teil des Strings zu extrahieren:

```Java
String name = "Max Mustermann";
String firstName = name.substring(0, 3); // "Max"
```

Hier beginnt der Vorname bei Index 0 und endet bei Index 3 (exklusive). Beachten Sie, dass die Endposition nicht mit extrahiert wird.

Substrings können auch verwendet werden, um bestimmte Teile von Strings zu vergleichen. In diesem Beispiel überprüfen wir, ob ein String mit einem bestimmten Präfix beginnt:

```Java
String sentence = "Java ist eine tolle Programmiersprache.";
boolean startsWithJava = sentence.startsWith("Java"); // true
```

Zuletzt können wir Substrings auch nutzen, um Strings zu manipulieren. Hier tauschen wir z.B. die Reihenfolge von Vor- und Nachname aus:

```Java
String name = "Max Mustermann";
String newFullName = name.substring(4) + " " + name.substring(0, 3); // "Mustermann Max"
```

## Tiefere Einblicke

Bei der Verwendung von `substring()` ist es wichtig zu beachten, dass der zurückgegebene Teil immer ein neues String-Objekt ist. Das bedeutet, dass der ursprüngliche String nicht verändert wird.

Außerdem können wir auch negative Indizes angeben, um von rechts nach links zu zählen. So würde `-2` z.B. das vorletzte Zeichen in einem String zurückgeben.

Es ist auch möglich, Substrings in Strings zu suchen. Hier können wir die Methode `indexOf()` verwenden, um die Position des gesuchten Substrings zu erhalten. Falls dieser Teilstring nicht existiert, wird `-1` zurückgegeben.

## Siehe Auch

- [Oracle-Dokumentation zu Substrings](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Java String-Referenz](https://www.javatpoint.com/java-string)
- [Baeldung - Stringschnitt in Java](https://www.baeldung.com/java-string-substring)