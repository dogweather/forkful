---
title:                "Verkettung von Zeichenketten"
html_title:           "Java: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir in die Details einsteigen, fragen Sie sich vielleicht: Warum sollte ich überhaupt Strings konkatentieren? Die Antwort ist einfach - das Konkatentieren von Strings ist eine grundlegende Fähigkeit, die Ihnen hilft, Texte zu verbinden und Daten zu manipulieren. Es ist ein wichtiger Bestandteil der Java-Programmierung und wird in vielen Anwendungen und Projekten verwendet.

## Wie geht das

Um Strings in Java zu konkatentieren, verwenden Sie den "+" Operator. Schauen wir uns ein Beispiel an:

```java
String str1 = "Hallo";
String str2 = "Welt";
String str3 = str1 + str2;
System.out.println(str3);
```

Die Ausgabe dieses Codes wird "HalloWelt" sein, da die beiden Strings miteinander verbunden wurden. Beachten Sie, dass Sie auch Variablen mit konkatentieren können, wie in der Variable "str3" im obigen Beispiel gezeigt.

Sie können auch mehr als zwei Strings miteinander verbinden, indem Sie einfach weitere "+" Operatoren zwischen ihnen verwenden, wie in diesem Beispiel:

```java
String str1 = "Ich";
String str2 = "bin";
String str3 = "ein";
String str4 = "Programmierer";
String str5 = str1 + " " + str2 + " " + str3 + " " + str4;
System.out.println(str5);
```

Die Ausgabe dieses Codes wird "Ich bin ein Programmierer" sein, da die fünf Strings miteinander verbunden wurden.

## Tiefeneintauchen

Es ist wichtig zu beachten, dass das Konkatentieren von Strings in Java sehr effizient ist. Dies liegt daran, dass in Java Strings als nicht-veränderbare Objekte behandelt werden, was bedeutet, dass jeder Vorgang, der an einem String durchgeführt wird, ein neues String-Objekt erstellt. Aus diesem Grund ist das Konkatentieren von Strings mit "+" in Java viel effizienter als in vielen anderen Programmiersprachen, da es die Anzahl der neuen String-Objekte minimiert, die erstellt werden müssen.

Sie können auch die "concat" Methode verwenden, um Strings zu konkatentieren, aber dies ist nicht so effizient wie der "+" Operator. Hier ist ein Beispiel:

```java
String str1 = "Hallo";
String str2 = "Welt";
String str3 = str1.concat(str2);
System.out.println(str3);
```

Die Ausgabe dieses Codes wird wieder "HalloWelt" sein, aber es wird ein zusätzliches String-Objekt erstellt, das die beiden Strings zusammenfügt. Wenn Sie jedoch mehr als zwei Strings konkatentieren möchten, ist es effizienter, einfach die "+" Operatoren zu verwenden.

## Siehe auch

- [Java String Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String Konkatentierung Tutorial](https://www.tutorialspoint.com/java/java_string_concat.htm)
- [Java Stringbuilder Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)