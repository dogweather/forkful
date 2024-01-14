---
title:                "Java: Unterstrings extrahieren"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
Substring-Extraktion kann in vielen Fällen eine hilfreiche Funktion sein, besonders wenn du mit längeren Strings arbeitest, die bestimmte Informationen enthalten, die du benötigst. Durch die Extraktion dieser Substrings kannst du deine Programmierarbeit vereinfachen und schnell die benötigten Daten erhalten.

## Wie geht es
Das Extrahieren von Substrings in Java ist einfach und erfordert nur ein paar Zeilen Code. Hier ist ein Beispiel, in dem wir den Vornamen aus einem String mit dem vollständigen Namen extrahieren:

```Java
String fullName = "Max Mustermann";
String firstName = fullName.substring(0, 3);

System.out.println(firstName);
```

Dieser Code wird "Max" ausgeben, da die `substring()`-Methode den Teil des Strings zwischen dem Startindex (0) und dem Endindex (3) zurückgibt. Beachte die Indexierung beginnt bei 0 und der Endindex ist exklusiv, also muss der Endindex um 1 erhöht werden, um den gewünschten Teil zu erhalten.

Um den Nachnamen zu extrahieren, könnten wir die `lastIndexOf()`-Methode verwenden, um den Index des Leerzeichens zwischen Vor- und Nachnamen zu finden:

```Java
int spaceIndex = fullName.lastIndexOf(" ");
String lastName = fullName.substring(spaceIndex + 1);

System.out.println(lastName);
```

Dieser Code gibt "Mustermann" aus, da wir den Teil des Strings erhalten, der nach dem Leerzeichen kommt.

## Tiefer eintauchen
Das Extrahieren von Substrings in Java kann mit verschiedenen Methoden erfolgen, je nachdem welche Informationen du benötigst. Du kannst auch die Länge des zu extrahierenden Teils angeben, indem du keinen Endindex angibst, sodass der Substring vom Startindex bis zum Ende des Strings zurückgegeben wird. Es ist auch möglich, nur einen Teil des Substrings zu erhalten, indem ein Start- und Endindex für den Teil des Substrings angegeben wird.

## Siehe auch
- [Java String Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [substring() Methode Erklärung und Beispiele](https://www.geeksforgeeks.org/java-string-substring-method-example/)
- [lastIndexOf() Methode Erklärung und Beispiele](https://www.geeksforgeeks.org/java-string-lastindexof-method-example/)